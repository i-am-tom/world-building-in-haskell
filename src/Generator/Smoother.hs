{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Generator.Smoother
  ( Smoother (..)
  , smooth
  
  , anneal
  , cluster
  ) where

import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Data.Nat (Nat)
import qualified Map
import Map (Map_)
import Numeric.Natural (Natural)

-- | A 'Smoother' defines an iterative operation over some 'Map'. Typically,
-- these are used /after/ some noise generation: the smoothing algorithm tries
-- to use the noise to produce an "organic" effect.
data Smoother (n :: Nat) (x :: Type)
  = Smoother
      { _step :: Map_ n (Maybe x) -> x -- ^ The smoothing algorithm
      , _size :: Natural               -- ^ How big a kernel do we need?
      }

-- | Repeatedly apply (for a given number of iterations) a smoothing function
-- to a map. Typically, the higher the number of iterations, the less "noisy"
-- the result.
smooth :: Int -> Smoother n x -> Map_ n x -> Map_ n x
smooth iterations _
  | iterations < 1 = id
smooth iterations smoother@Smoother{..}
  = smooth (iterations - 1) smoother
  . fmap _step
  . Map.kernels _size

-- | Assign each cell to the average of the values in its kernel. The more this
-- smoother is iterated, the closer every value will come to converging on a
-- common number. The parameters dictate the size of the kernel and the value
-- to be used for "out-of-bounds" cells.
anneal :: Fractional x => Natural -> x -> Smoother n x
anneal _size border
  = Smoother
      { _step = \kernel -> do
          let corrected = fmap (fromMaybe border) kernel
          sum corrected / fromIntegral (length corrected)

      , ..
      }

-- | Given a map of 'Bool' values, this smoother assigns each cell to the most
-- common value among its neighbours. If the difference between the number of
-- 'True' and 'False' neighbours is less than @2 * tolerance@, the value is
-- unchanged.
cluster :: Int -> Smoother n Bool
cluster tolerance = Smoother
  { _step = \(toList -> kernel) -> do
      let middle :: Int
          middle = length kernel `div` 2

          original :: Maybe Bool
          original = kernel !! middle

          trues :: Int -- How many true neighbours do I have?
          trues = length (filter (fromMaybe True) (delete original kernel))

      if | trues < middle - tolerance -> False
         | trues > middle + tolerance -> True

           -- Of course, the bool should always be present in the centre of the
           -- kernel, as it's always sampled from /inside/ the map.
         | otherwise -> fromMaybe True original

  , _size = 1
  }
