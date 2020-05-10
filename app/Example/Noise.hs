{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Example.Noise where

import qualified Codec.Picture as Image
import Data.Traversable (for)
import qualified Generator
import qualified Map
import Map ((?!), Coordinate, Map)
import qualified Render

main :: IO ()
main = do
  let noises :: [ Coordinate 2 -> Maybe (IO (Map 2 Double)) ]
      noises = [ Generator.uniform, Generator.normal, Generator.perlin ]

  tiles <- for noises \noise ->
    case noise (Map.pos @2 80 80) of
      Just success -> success
      Nothing      -> error "Uh oh!"

  let pick :: Int -> Int -> Image.Pixel8
      pick x y = do
        let tile = tiles
              !! (x `div` 85)
              ?! Map.pos @2 (x `mod` 85) y
        
        case tile of
          Just value -> floor (value * 255)
          Nothing    -> 255

  Image.writePng "examples/01-noise.png"
    $ Render.scale 2
    $ Image.generateImage pick 250 80
