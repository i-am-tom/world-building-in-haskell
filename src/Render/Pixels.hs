{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Render.Pixels where

import Codec.Picture (Image (..), Pixel, Pixel8, PixelRGB8 (..))
import qualified Codec.Picture as Image
import Data.Bool (bool)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Vect (Vect (..))
import qualified Map
import Map ((?!), Map)

-- | Render a @2@-dimensional image by translating each cell of a @Map@ into a
-- pixel value. The resulting image can be scaled using 'Map.scale' if you're
-- working with small dimensions.
pixels :: forall x p. Pixel p => (x -> p) -> Map 2 x -> Image p
pixels renderer (fmap renderer -> pixelMap)
  = Image.generateImage choose width height
  where
    choose :: Int -> Int -> p
    choose x y = fromJust (pixelMap ?! Map.pos @2 x y)

    width :. height :. Nil = Map.dimensions pixelMap

-- | A monochrome renderer for 'Bool' maps. 'True' maps to black, 'False' to
-- white.
blackAndWhite :: Bool -> Pixel8
blackAndWhite = bool 255 0

-- | A monochrome renderer for 'Bool' maps. 'True' maps to black, 'False' to
-- white.
greyscale :: Double -> Pixel8
greyscale = max 0 . min 255 . floor . (* 128) . succ

-- | I didn't have a better name for this, so hopefully the intuition makes
-- sense. Given a list of thresholds and colours, draw a given value in the
-- colour of the first threshold it passes. If these colours are chosen to go
-- from, say, black to red, the effect is like a heat map. However, if these
-- values are altitudes, you can use this to draw terrain maps.
heatmap :: [( Double, PixelRGB8 )] -> Double -> PixelRGB8
heatmap thresholds value
  = case find match thresholds of 
      Just ( _, colour ) -> colour
      Nothing            -> Image.PixelRGB8 0 0 0
  where match ( threshold, _ ) = value > threshold
