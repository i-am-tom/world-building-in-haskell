{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Example.Mountains where

import qualified Codec.Picture as Image
import qualified Generator
import qualified Map
import qualified Render

main :: IO ()
main = do
  board <- case Generator.perlin (Map.pos @2 480 480) of
    Just success -> success
    Nothing      -> error "Uh oh!"

  let snow         = (0.85, Image.PixelRGB8 255 255 255)
      mountains    = ( 0.5, Image.PixelRGB8 200 200 200)
      forest       = ( 0.1, Image.PixelRGB8 116 151  62)
      land         = (   0, Image.PixelRGB8 139 181  74)
      sand         = (-0.1, Image.PixelRGB8 227 221 188)
      shallowWater = (  -2, Image.PixelRGB8 156 213 226)
      depths       = ( -25, Image.PixelRGB8  74 138 125)

      heatmap :: Double -> Image.PixelRGB8
      heatmap = Render.heatmap
        [ snow
        , mountains
        , forest
        , land
        , sand
        , shallowWater
        , depths
        ]

  Image.writePng "examples/07-mountains.png"
    $ Render.pixels heatmap board
