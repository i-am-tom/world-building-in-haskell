{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Example.Annealing where

import qualified Codec.Picture as Image
import qualified Generator
import qualified Map
import qualified Render
import Map ((?!))

main :: IO ()
main = do
  initial <- case Generator.uniform (Map.pos @2 80 80) of
    Just board -> board
    Nothing    -> error "Uh oh!"

  let smoother kernel iterations
        = Generator.smooth iterations
            (Generator.anneal kernel 0) initial
  
      tiles
        = [ smoother k i
          | k <- [  1, 2  ]
          , i <- [ 0 .. 4 ]
          ]

  let pick :: Int -> Int -> Image.Pixel8
      pick x y = do
        let tile = tiles
              !! ((y `div` 85) * 5 + x `div` 85)
              ?! Map.pos @2 (x `mod` 85) (y `mod` 85)
        
        case tile of
          Just value -> floor (value * 255)
          Nothing    -> 255

  Image.writePng "examples/02-annealing.png"
    $ Render.scale 2
    $ Image.generateImage pick 420 165
