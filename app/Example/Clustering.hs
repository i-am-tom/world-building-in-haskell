{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Example.Clustering where

import qualified Codec.Picture as Image
import qualified Generator
import qualified Map
import qualified Render
import Map ((?!))

main :: IO ()
main = do
  initial <- case Generator.uniform (Map.pos @2 80 80) of
    Just success -> success
    Nothing      -> error "Uh oh!"

  let iterator sensitivity iterations
        = Generator.smooth iterations (Generator.cluster sensitivity)
        $ Generator.boolify 0 initial
  
      tiles
        = [ iterator k i
          | k <- [ 0 .. 2 ]
          , i <- [ 0 .. 4 ]
          ]

  let pick :: Int -> Int -> Image.Pixel8
      pick x y = do
        let tile = tiles
              !! ((y `div` 85) * 5 + x `div` 85)
              ?! Map.pos @2 (x `mod` 85) (y `mod` 85)
        
        case tile of Just True -> 0
                     _         -> 255

  Image.writePng "examples/03-clustering.png"
    $ Render.scale 2
    $ Image.generateImage pick 420 250
