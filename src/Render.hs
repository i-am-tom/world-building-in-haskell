{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Render
  ( Mapping
  , Sprites
  , Tiles (..)

  , loadMapping
  , loadSprites

  , mappedSprites
  , sprites

  , pixels

  , blackAndWhite
  , heatmap
  , greyscale

  , isometric

  , scale
  ) where

import Codec.Picture (Image (..), Pixel)
import qualified Codec.Picture as Image
import Numeric.Natural (Natural)
import Render.Isometric (Tiles (..), isometric)
import Render.Pixels (blackAndWhite, greyscale, heatmap, pixels)
import Render.Sprites (Mapping, Sprites, loadMapping, loadSprites, mappedSprites, sprites)

-- | Scale an image by an integer factor.
scale :: Pixel p => Natural -> Image p -> Image p
scale (fromIntegral -> factor) image@Image{..}
  = Image.generateImage scaler (factor * imageWidth) (factor * imageHeight)
  where scaler i j = Image.pixelAt image (i `div` factor) (j `div` factor)
