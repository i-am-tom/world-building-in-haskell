{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Map.Type where

import qualified Control.Lens.Indexed as Ix
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty ((!!), NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Nat (Nat (..))
import Data.Nat.Extra (Unarise)
import Data.Traversable (for)
import Data.Vect (Vect (..))
import qualified GHC.TypeLits as TL
import Prelude hiding ((!!), iterate)

-- | A map is an @n@-dimensional matrix indexed by the number of dimensions and
-- the type contained in every "cell".
--
-- Note that we don't index by the dimensions themselves: because operations
-- such as kernels don't necessarily have to stay "within the boundaries",
-- we're OK with being able to point to coordinates "off-grid".
data Map_ (n :: Nat) (x :: Type) where
  Axis :: NonEmpty (Map_ n x) -> Map_ ('S n) x
  Cell :: x -> Map_ 'Z x

type Map (n :: TL.Nat)
  = Map_ (Unarise n)

deriving instance Eq x => Eq (Map_ n x)
-- deriving instance Show x => Show (Map_ n x)
deriving instance Foldable (Map_ n)
deriving instance Functor (Map_ n)
deriving instance Traversable (Map_ n)

instance (Nested n x o, Show o)
    => Show (Map_ n x) where
  show xs = "MAP " <> show (toNestedLists xs)

instance Ix.FoldableWithIndex (Vect n Int) (Map_ n) where
  ifoldMap f = \case
    Axis xs -> xs & Ix.ifoldMap \i -> Ix.ifoldMap \is -> f (i :. is)
    Cell x  -> f Nil x

instance Ix.FunctorWithIndex (Vect n Int) (Map_ n) where
  imap f = \case
    Axis xs -> Axis $ xs & Ix.imap \i -> Ix.imap \is -> f (i :. is)
    Cell x  -> Cell (f Nil x)

instance Ix.TraversableWithIndex (Vect n Int) (Map_ n) where
  itraverse f = \case
    Axis xs -> fmap Axis $ xs & Ix.itraverse \i -> Ix.itraverse \is -> f (i :. is)
    Cell x  -> fmap Cell (f Nil x)

-- | Look up the value at a given coordinate on the 'Map_'. Note that the
-- coordinate can reference a position outside the boundaries of the 'Map_', so
-- the operation may find 'Nothing'.
(?!) :: Map_ n x -> Vect n Int -> Maybe x
(?!) (Axis xs) ((fromIntegral -> i) :. is)
  | i >= 0 && i < length xs = (xs !! i) ?! is
(?!) (Cell x) Nil = Just x
(?!)  _       _   = Nothing

infixr 3 ?!

-- | Create a new 'Map_' with the given dimensions. Each cell is filled by
-- passing the cell's coordinate to the given function. All dimensions must be
-- non-zero; if you need a zero dimension, it can just be ommitted (as we'll
-- never be able to place anything on the map).
create :: Vect n Int -> (Vect n Int -> x) -> Maybe (Map_ n x)
create Nil f = Just (Cell (f Nil))
create (i :. _ ) _
  | i <= 0 = Nothing
create (i :. is) f = do
  indices       <- nonEmpty [ 0 .. i - 1 ]
  subdimensions <- for indices \n ->
    create is \ns -> f (n :. ns)

  pure (Axis subdimensions)

-- | Get the dimensions of a 'Map_'. This assumes that all side lengths are
-- equal (which is true of all 'Map_' values obtained via 'create').
dimensions :: Map_ n x -> Vect n Int
dimensions = \case
  Axis (r :| rs) -> length rs + 1 :. dimensions r
  Cell  _        -> Nil

-- | Convert a 'Map_' into a nested array. This /is/ a forgetful operation, as
-- we can't tell how many dimensions were in the map that produced
-- @[[[Bool]]]@: was it @3@ dimensions of @Bool@, or @2@ of @[Bool]@?
class Nested (dimensions :: Nat) (cells :: Type) (result :: Type)
    | dimensions cells -> result
    , dimensions result -> cells where
  toNestedLists :: Map_ dimensions cells -> result

instance x ~ o => Nested 'Z x o where
  toNestedLists (Cell x) = x

instance (Nested n x os, o ~ [os]) => Nested ('S n) x o where
  toNestedLists (Axis rs) = map toNestedLists (NonEmpty.toList rs)
