{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Map.Kernel where

import Control.Lens.Indexed (FunctorWithIndex (..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Vect (Vect (..))
import Map.Coordinate ((.+))
import Map.Type ((?!), Map_ (..))
import qualified Map.Type as Map
import Numeric.Natural (Natural)

-- | Construct kernels for every point in the map. Each cell will be replaced
-- by a "submap" containing itself and all the points within @radius@ cells of
-- itself. This can be useful if you want to "smooth" an image, or detect
-- edges, or are trying to implement algorithms like marching squares.
kernels :: Natural -> Map_ n x -> Map_ n (Map_ n (Maybe x))
kernels (fromIntegral -> radius) board = do
  let side :: Int
      side = radius * 2 + 1

      dimensions :: Map_ n x -> Vect n Int
      dimensions = \case
        Axis (r :| _) -> side :. dimensions r
        Cell  _       -> Nil

  board & imap \global _ -> do

    -- 'fromJust' is safe here as the radius will always be a natural number.
    -- This means the smallest it could possibly be is @0 * 2 + 1 = 1@, which
    -- is within our bounds. This is tricky to explain to GHC without an awful
    -- lot more type machinery, though.
    fromJust $ Map.create (dimensions board) \local -> do
      let centred = fmap (subtract radius) local
      board ?! centred .+ global
