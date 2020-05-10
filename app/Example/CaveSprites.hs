{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Example.CaveSprites where

import qualified Codec.Picture as Image
import Control.Lens.Indexed (imap)
import Example.Cave2d (cave)
import qualified Map
import Map (Map, Vect (..))
import qualified Render

main :: IO ()
main = do
  board   <- cave (Map.pos @2 40 40)
  mapping <- Render.loadMapping "tile-mappings.json"
  sprites <- Render.loadSprites "sprites.png" 16 1

  let kernel :: Map 2 (Maybe Bool) -> Int
      kernel = sum . imap \(x :. y :. Nil) value ->
        case value of
          Just False -> 0
          _          -> 2 ^ (8 - (y * 3 + x))

  Image.writePng "examples/05-cave.png"
    $ Render.mappedSprites mapping sprites kernel
    $ Map.kernels 1 board
