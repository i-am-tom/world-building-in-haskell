{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Example.Cave3d where

import qualified Codec.Picture as Image
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NonEmpty
import Example.Cave2d (cave)
import qualified Map
import qualified Render

main :: IO ()
main = do
  world <- cave (Map.pos @3 40 40 40)

  let toRGB8 :: Image.Pixel8 -> Image.PixelRGB8
      toRGB8 x = Image.PixelRGB8 x x x

      frames = Map.slice @2 world <&> \layer ->
        Render.scale 8
          $ Render.pixels (toRGB8 . Render.blackAndWhite)
          $ layer
      
      animation = Image.writeGifAnimation "examples/06-cave.gif"
        10 Image.LoopingForever (NonEmpty.toList frames)

  case animation of
    Left message -> error message
    Right gif    -> gif
