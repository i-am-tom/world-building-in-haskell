{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Generator.WaveFunctionCollapse
  ( Cell
  , Connections
  , Prop

  , collapse
  , connect
  , runAll
  , runMany
  , runOne
  ) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Lens ((<>~), (^@..), (.~), (^..), _1, _2, _head, _last, itraversed, to)
import Control.Lens.Indexed (ifor_)
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logic (LogicT)
import qualified Control.Monad.Logic as Logic
import Control.Monad.Primitive (PrimMonad (..))
import Data.Foldable (asum, for_, traverse_)
import Data.Function (on)
import Data.Kind (Type)
import Data.List.NonEmpty ((!!))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Ap (..))
import Data.Nat.Extra (Twice)
import Data.Nat.Extra (Unarise)
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Unique (Unique, newUnique)
import Data.Vect (Make (..), Vect (..), zipWithA_)
import GHC.Exts (groupWith, sortWith)
import qualified GHC.TypeLits as TL
import qualified Map
import Map ((?!), Map_)
import Prelude hiding ((!!), read, until)
import qualified Test.QuickCheck.Gen as Gen

-- | There are several ways to express the WaveFunctionCollapse, but I find
-- propagators to be the most natural fit - in essence, WFC is really just one
-- specialised form of a propagator network, albeit one that uses randomness
-- and ends up a little less "pure".
--
-- It's probably more helpful to skip straight down to 'collapse', and come
-- back to the internals later, once you have an intuition for what's going on.
newtype Prop (x :: Type)
  = Prop { runProp :: LogicT IO x }
  deriving newtype (Functor, Alternative, Applicative, Monad, MonadIO)
  deriving (Semigroup, Monoid) via (Ap (LogicT IO) x)

instance PrimMonad Prop where
  type PrimState Prop = PrimState IO
  primitive = Prop . Logic.lift . primitive

-- | Run a propagator computation, returning an array of results. Note that, in
-- most WFC applications, there are a /lot/ of possible permutations (we're
-- talking millions), so this operation may be extremely slow. It's probably
-- more sensible to use @runOne@ or @runMany@ and just take a few at a time.
runAll :: Prop x -> IO [ x ]
runAll = Logic.observeAllT . runProp

-- | Run a propagator computation, returning the first successful permutation
-- (if it exists). This has the advantage of terminating immediately, and so
-- will perform much faster than @runAll@.
runOne :: Prop x -> IO (Maybe x)
runOne = fmap listToMaybe . Logic.observeManyT 1 . runProp

-- | Run a propagator computation, returning a given number of results (or
-- fewer if there aren't that many successes).
runMany :: Int -> Prop x -> IO (Maybe x)
runMany count = fmap listToMaybe . Logic.observeManyT count . runProp

--------------------------------------------------

-- | Cells are the basic value storage in a propagator network. In our
-- specialised version, a cell holds a set of "possible values", and we
-- eliminate values from this set over time. We consider a value "collapsed"
-- when only one possible candidate remains.
data Cell (x :: Type)
  = Cell
      { _id  :: Unique
      , _ref :: MutVar (PrimState Prop) (Set x, Prop ())
      }

instance Eq  (Cell x) where (==)    = (==)    `on` _id
instance Ord (Cell x) where compare = compare `on` _id

-- | Create a new 'Cell' containing the given list of possible values. 
fill :: Ord x => [ x ] -> Prop (Cell x)
fill (Set.fromList -> xs) = do
  _id  <- liftIO newUnique
  _ref <- Prim.newMutVar (xs, mempty)

  pure Cell{..}

-- | Read the current possible candidates for a given 'Cell'.
read :: Cell x -> Prop (Set x)
read = fmap fst . Prim.readMutVar . _ref

-- | Register a callback on a 'Cell'. Every time that 'Cell' is updated, its
-- remaining candidates will be passed to the function, which should in turn
-- try to write updates to other cells, and thus trigger even more callbacks.
--
-- Note that we can backtrack this operation - if something goes wrong, we
-- return the cell to its original state.
watch :: Cell x -> (Set x -> Prop ()) -> Prop ()
watch cell@Cell{..} callback = do
  let propagator = read cell >>= callback
  ( _, propagators ) <- Prim.readMutVar _ref

  Prim.modifyMutVar _ref (_2 <>~ propagator) *> propagator
    <|> Prim.modifyMutVar _ref (_2 .~ propagators) *> empty

-- | Write an update to a 'Cell'. Here, we write a set of possibilities, which
-- we call @news@. We then read the 'Cell' to find the @olds@. The
-- /intersection/ of the two is stored, and so the list of candidates is
-- reduced. If any values /are/ removed, the callbacks registered to this
-- 'Cell' will be fired.
--
-- Note that, if a 'write' leaves a 'Cell' with no remaining candidates, this
-- branch of the computation is deemed "unsuccessful", and we backtrack and try
-- different branches until we find a successful result.
--
-- We can also backtrack this operation: if something goes wrong, we restore a
-- cell to its previous value.
write :: Ord x => Cell x -> Set x -> Prop ()
write Cell{..} news = do
  ( olds, propagators ) <- Prim.readMutVar _ref
  let joined = Set.intersection news olds

  guard (not $ Set.null joined)

  unless (Set.size joined == Set.size olds) do
    Prim.modifyMutVar _ref (_1 .~ joined) *> propagators
      <|> Prim.modifyMutVar _ref (_1 .~ olds) *> empty

--------------------------------------------------

-- | Initialise a map in which every cell is a 'Cell' storing the given
-- possibilities. At the beginning of a WFC computation, all cells should have
-- the possibility of being "any" value, and we eliminate possibilities as we
-- go.
initialise :: Ord x => Vect n Int -> [ x ] -> Maybe (Prop (Map_ n (Cell x)))
initialise dimensions = fmap sequence . Map.create dimensions . const . fill

-- | Read the collapsed values from a map. This operation is unsafe because it
-- assumes every cell has exactly one remaining candidate. If there are more,
-- the result will not necessarily obey the constraints given in the
-- computation. If there are fewer, this will cause a runtime exception.
unsafeReadMap :: Map_ n (Cell x) -> Prop (Map_ n x)
unsafeReadMap = traverse (fmap Set.findMax . read)

-- | State the set of possible values around the border of the map. Typically,
-- this is useful if the border needs to be solid, or give the impression of an
-- "island", et cetera. The function requires a vector with twice as many
-- elements as the number of dimensions. These elements correspond to the
-- following borders:
--
-- @
--   n == 1 => [ Right, Left ]
--   n == 2 => [ Right, Left, Bottom, Top ]
--   n == 3 => [ Right, Left, Bottom, Top, Down, Up ]
-- @
--
-- The reason is that these values should be the "opposite" to the order of the
-- values returned from the user-supplied constraint function (see 'collapse'
-- below). When we apply the given constraint value to a 'Nothing', the "left"
-- value becomes the "right" border, the "bottom" value becomes the "top"
-- border, etc. Intuitively, this is because we treat everything off the map as
-- 'Nothing', so the top border is indeed "underneath" the 'Nothing' value.
surround :: Ord x => Vect (Twice n) (Set x) -> Map_ n (Cell x) -> Prop ()
surround xs = \case
  Map.Axis (NonEmpty.toList -> rs) -> do
    let (x :. y :. zs) = xs

    for_ (rs ^.. _last . traverse) \cell -> write cell x
    for_ (rs ^.. _head . traverse) \cell -> write cell y

    traverse_ (surround zs) rs

  Map.Cell _ -> pure ()

-- | Find the values in the directly-neighbouring cells to a given coordinate.
-- The order will be left, right, top, bottom, up, down, and "so on" (perhaps
-- your 4D intuition is better than mine). This follows the order of the
-- constraint function given to 'collapse', so we can zip them together.
neighbours :: Map_ n x -> Vect n Int -> Vect (Twice n) (Maybe x)
neighbours board@(Map.Axis rows) (x :. xs)
  =  (board ?! pred x :. xs)
  :. (board ?! succ x :. xs)
  :. (neighbours (rows !! x) xs)
neighbours (Map.Cell _) _ = Nil

-- | Run a monadic action 'until' the given predicate action yields a 'True'
-- value. We only need it for 'IO' (well, 'Prop'), but it's more general.
until :: Monad m => m Bool -> m () -> m ()
until predicate action = predicate >>= \case
  True  -> pure ()
  False -> action *> until predicate action

-- | The actual WaveFunctionCollapse implementation. I've written it in terms
-- of propagators, as that's the way that makes most intuitive sense to me.
--
-- Given a number of dimensions, a list of possible cell values, and a
-- constraint set, produce a map of values in which the constraints are
-- satisfied. The constraints are /defined/ using the third argument: the
-- @connections@ function. This function should take a value (which, if it's
-- not on the map, will be @Nothing@), and return the values that are allowed
-- to be placed next to it. These values are ordered as left, right, top,
-- bottom, up, down... and so on, into dimensions I can't visualise.
--
-- If you're curious, see the source code for in-line explanations of what's
-- going on.
collapse
  :: forall n x
   . Applicative (Vect (Twice n))
  => Ord x
  => Vect n Int
  -> [ x ]
  -> (Maybe x -> Vect (Twice n) (Set x))
  -> Prop (Map_ n x)

collapse dimensions candidates connections = do

  -- First, we create a map where every cell contains a 'Cell'. Each 'Cell',
  -- initially, will have the possibility of being /any/ of the given
  -- candidates. Hopefully, many of these candidates will be eliminated as we
  -- start collapsing.
  initial <- fromMaybe empty (initialise dimensions candidates)

  -- We pass 'Nothing' to the @connections@ function, to return the neighbours
  -- of a cell off the map. The left neighbour of 'Nothing' is therefore the
  -- /right/ border of the map. The neighbour above 'Nothing' is the /bottom/
  -- border of the map, and so on.
  surround (connections Nothing) initial

  -- Now, we set up the constraints for each cell on the map. The neat thing
  -- about the propagator model is we can express operations in terms of each
  -- cell and its neighbours, rather than trying to keep track of this
  -- "globally".
  ifor_ initial \position cell -> do

    -- The immediate neighbours of a cell are the only things it can directly
    -- affect in this formulation of the algorithm.
    let targets = neighbours initial position

    watch cell \(Set.toList -> current) -> do

      -- This might look a bit confusing, so we'll break it down. @current@
      -- here is a list of all remaining possibilities within a given cell. For
      -- each of these possibilities, we figure out all the possible
      -- neighbouring values. Then, we union together all the possibilities for
      -- each neighbour across all the candidates.
      --
      -- In other words, if any value in this cell allows a particular value in
      -- a particular neighbour, then that value is still a possibility. If
      -- none of the remaining values allow that value, then that value can be
      -- eliminated from the cell.
      let neighbourUpdates
            = fmap (foldl1 Set.union)
            . traverse (connections . Just) -- TODO: check this, it might have broken things
            $ current

      zipWithA_ (traverse_ . flip write) neighbourUpdates targets

  let shuffle :: forall a. [ a ] -> Prop [ a ]
      shuffle = liftIO . Gen.generate . Gen.shuffle

      -- This is definitely not the most efficient way to check whether the map
      -- has collapsed (ideally, you'd use a queue to keep track of which cells
      -- have and haven't collapsed), but I've done it this way just so we
      -- don't distract too much from the core implementation. A map has
      -- collapsed if all cells have been whittled down to having only one
      -- remaining possibility.
      collapsed :: Prop Bool
      collapsed = fmap (all \x -> Set.size x == 1) (traverse read initial)

  -- Now we've specified all the constraints, the idea is that we find the set
  -- of "most collapsed but not yet fully collapsed" cells (e.g. all the cells
  -- that contain the joint-lowest number of possibilities), pick one of these
  -- cells at random, and collapse it to a random remaining possibility. This
  -- should trigger more neighbouring collapse, and we repeat this process
  -- until we've hit a collapsed state.
  until collapsed do
    snapshot <- traverse read initial

        -- Each coordinate and its number of remaining candidates.
    let sizes :: [( Vect n Int, Int )]
        sizes = snapshot ^@.. itraversed . to Set.size

        -- All coordinates (and their sizes) that haven't yet collapsed.
        uncollapsed :: [( Vect n Int, Int )]
        uncollapsed = filter ((/= 1) . snd) sizes

        -- All coordinates grouped by how many remaining candidates they have
        -- (in ascending order).
        remainders :: [[( Vect n Int, Int )]]
        remainders = groupWith snd (sortWith snd uncollapsed)

        -- The set of coordinates with the join lowest number of remaining
        -- candidates (that haven't already collapsed). @head@ is safe here as
        -- our @until collapsed@ check guarantees there's a non-zero number of
        -- "uncollapsed" candidates remaining.
        focus :: [ Vect n Int ]
        focus = map fst (head remainders)

    -- We shuffle the candidates, and try collapsing the first. If that doesn't
    -- work, we try collapsing the second, and so on. We can backtrack along
    -- the list of coordinates until we find one that results in a successful
    -- collapse, and we have our answer!
    shuffle focus >>= asum . map \coordinate ->
      case initial ?! coordinate of
        Just cell -> do

          -- Read all the possible values, shuffle them, and then try each
          -- remaining value in turn until we find a successful collapse.
          values <- read cell >>= shuffle . Set.toList
          asum (fmap (write cell . Set.singleton) values)

        Nothing -> -- This should be impossible.
          pure ()

  -- Now the map has collapsed, and we haven't at some point hit an 'empty' due
  -- to a cell with no remaining candidates, this operation is safe. We can
  -- read the single remaining candidate from each cell, and finally produce
  -- our collapsed map. Hooray!
  unsafeReadMap initial

--------------------------------------------------

-- A little convenience using "GHC.TypeLits". If you statically know how many
-- dimensions you're working with, this means you can use numeric literals in
-- your type, which is a bit tidier.
type Connections (n :: TL.Nat) (x :: Type)
  = Vect (Unarise n) x

-- | Just like 'Map.pos', but for vectors of any type. You're most likely only
-- going to need this for your constraint function, though.
connect :: forall n x k. Make (Unarise n) x k => k
connect = make @(Unarise n)
