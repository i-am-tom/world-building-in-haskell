{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Render.Isometric where

import Codec.Picture (Image, PixelRGBA8 (..))
import qualified Codec.Picture as Image
import qualified Codec.Picture.Types as Image (createMutableImage, freezeImage)
import Control.Lens ((^@..), itraversed)
import Data.Foldable (for_)
import Data.Vect (Vect (..))
import GHC.Exts (sortWith)
import qualified Map
import Map (Coordinate, Map)
import System.FilePath ((</>))

-- | Configuration for an isometric tileset. Unlike a sprite sheet, because of
-- the irregular shape, we'll store the tiles as separate images, and load them
-- from a given directory.
data Tiles
  = Tiles
      { _x, _y, _z :: Int
      , _directory :: FilePath
      }
  deriving (Eq, Ord, Show)

-- | Calculate the dimensions of an isometric render's canvas. We assume that
-- tiles overlap by the height of the tile depth, and so the height is
-- calculated by assuming tiles are @full height - depth@ tall, and adding an
-- extra @depth@ value to the bottom.
canvasSize :: Coordinate 3 -> Tiles -> Coordinate 2
canvasSize ( x :. y :. z :. Nil ) Tiles{..} = width :. height :. Nil
  where
    width :: Int
    width = ( x + y ) * _x `div` 2

    height :: Int
    height = ( x + y ) * ( _y - _z ) `div` 2 + ( z * _z )

-- | Calculate the top-left point (ignoring depth) of a tile at the given
-- coordinate when projected onto the output image. From this point, we can
-- draw the rest.
position :: Coordinate 3 -> Coordinate 3 -> Tiles -> Coordinate 2
position ( _ :. h :. _ :. Nil ) ( x :. y :. z :. Nil ) Tiles{..}
  = left :. top :. Nil
  where
    left :: Int
    left = ( h - 1 + x - y ) * _x `div` 2

    top :: Int
    top = ( x + y ) * ( _y - _z ) `div` 2 + ( z * _z )

-- | Arrange the tiles in render order. We should render the tiles at the
-- "back" before the tiles at the "front" to avoid overlap.
queue :: Map 3 x -> [( Coordinate 3, x )]
queue xs = sortWith (zIndex . fst) (xs ^@.. itraversed)
  where
    zIndex :: Coordinate 3 -> Int
    zIndex ( x :. y :. _ :. Nil ) = x + y

-- | Render a @3@-dimensional map to an isometric canvas. The @renderer@
-- function should map each cell to the tile file.
isometric :: Tiles -> (x -> FilePath) -> Map 3 x -> IO (Image PixelRGBA8)
isometric tiles@Tiles{..} renderer (fmap renderer -> board) = do
  let width :. height :. Nil = canvasSize dimensions tiles
      dimensions             = Map.dimensions board

  canvas <- Image.createMutableImage width height (PixelRGBA8 0 0 0 0)

  for_ (queue board) \(coordinate, path) -> do
    let ox :. oy :. Nil = position dimensions coordinate tiles
        offsets = do
          x <- [ 0 .. _x - 1 ]
          y <- [ 0 .. _y - 1 ]

          pure (x, y)

    sprite <- Image.readPng (_directory </> path <> ".png") >>= \case
      Right success -> pure (Image.convertRGBA8 success)
      Left message  -> error message

    for_ offsets \( x, y ) ->
      let x' = ox + x
          y' = oy + y

      in case Image.pixelAt sprite x y of
        PixelRGBA8 _ _ _ 0 -> pure ()
        solid              -> Image.writePixel canvas x' y' solid

  Image.freezeImage canvas
