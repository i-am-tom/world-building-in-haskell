{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Example.Cave2d where

import qualified Codec.Picture as Image
import qualified Generator
import qualified Map
import Map (Map_ (..), Vect)
import qualified Render

cave :: Vect n Int -> IO (Map_ n Bool)
cave dimensions = do
  board <- case Generator.uniform dimensions of
    Just success -> success
    Nothing      -> error "A dimension isn't greater than 0!"

  let bools   = Generator.boolify 0 board
      cluster = Generator.cluster 0

  pure (Generator.smooth 2 cluster bools)

main :: IO ()
main = do
  board <- cave (Map.pos @2 40 40)

  Image.writePng "examples/04-cave.png"
    $ Render.scale 4
    $ Render.pixels Render.blackAndWhite
    $ board
