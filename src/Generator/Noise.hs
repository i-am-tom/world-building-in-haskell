{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Generator.Noise where

import Data.Functor ((<&>))
import Data.Random.Normal (normalIO')
import Data.Vect (Vect (..))
import Map (Coordinate, Map, Map_)
import qualified Map as Map
import qualified Numeric.Noise.Perlin as Perlin
import System.Random (randomIO, randomRIO)

-- | Populate a map with normally-distributed noise using the system's random
-- number generator. The noise will be produced with a mean of @0@ and a
-- standard deviation of @1@.
normal :: Vect n Int -> Maybe (IO (Map_ n Double))
normal size = fmap sequenceA (Map.create size (const noise))
  where noise = normalIO' (0, 1)

-- | Populate a map with uniformly-distributed noise using the system's random
-- number generator. The values produced will be from @[-1, 1]@.
uniform :: Vect n Int -> Maybe (IO (Map_ n Double))
uniform size = fmap sequenceA (Map.create size (const noise))
  where noise = randomRIO (-1, 1)

-- | Convert a 'Map' of 'Double' values into a 'Map' of 'Bool' values by
-- comparing each cell to a given threshold. If the value of the cell exceeds
-- the threshold, it becomes true.
boolify :: Double -> Map_ n Double -> Map_ n Bool
boolify threshold = fmap (> threshold)

-- | Two-dimensional Perlin noise (for three-dimensional maps).
perlin :: Coordinate 2 -> Maybe (IO (Map 2 Double))
perlin dimensions = do
  generators <- Map.create dimensions \(x :. y :. Nil) function ->
    Perlin.noiseValue function ( fromIntegral x, fromIntegral y, 0 )

  pure do
    function <- randomIO <&> \seed ->
      Perlin.perlin seed 4 0.005 0.5

    pure (fmap ($ function) generators)
