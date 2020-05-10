{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Render.Sprites
  ( mappedSprites
  , sprites

  , Mapping
  , getMapping
  , getAllMappings
  , loadMapping

  , Sprites
  , loadSprites
  , getSprite
  , getAllSprites
  ) where

import Codec.Picture (DynamicImage, Image, Pixel, PixelRGBA8 (..))
import qualified Codec.Picture as Image
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Vector ((!?), Vector)
import Data.Vect (Vect (..))
import GHC.Generics (Generic)
import qualified Map
import Map ((?!), Map, Map_)
import Prelude hiding (lookup)

-- | Render some sprites using an external mapping. The third argument will
-- take the value in a cell and map it to an index within the mapping, which
-- will correspond to the sprite to use. This is helpful when you're working
-- with a large set of sprites.
mappedSprites :: Mapping -> Sprites -> (x -> Int) -> Map 2 x -> Image PixelRGBA8
mappedSprites mapping spritesheet f
  = sprites spritesheet (getMapping mapping . f)

-- | Render a map to an image using an external sprite sheet. Every cell is
-- mapped to a coordinate within the sprite sheet, and that sprite is rendered
-- to that @x@/@y@ position in the final image.
sprites :: Sprites -> (x -> ( Int, Int )) -> Map 2 x -> Image PixelRGBA8
sprites reference@Sprites{..} position board = do
  let cellToTileImage   = Image.convertRGBA8 . getSprite reference . position
      width :. height :. Nil = Map.dimensions board

      pixel x y = do
        let (spriteX, i) = x `divMod` _size
            (spriteY, j) = y `divMod` _size

        fromMaybe (Image.PixelRGBA8 0 0 0 0) do
          board ?! Map.pos @2 spriteX spriteY <&> \cell ->
            Image.pixelAt (cellToTileImage cell) i j

  Image.generateImage pixel (width * _size) (height * _size)

-- | When we're choosing tiles based on kernels, the mapping from kernels to
-- tiles can become very large. So we don't clutter our Haskell code, we can
-- store the 'Mapping' in an external file. This file should contain a JSON
-- object, with two keys:
--
-- - @positions@, which holds a vector of sprite positions. Users can look up
--   these positions based on their indices.
-- - @fallback@, which is returned if the user-supplied index is out-of-bounds.
data Mapping
  = Mapping
      { fallback  :: ( Int, Int )
      , positions :: Vector ( Int, Int )
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Load a mapping from disk.
loadMapping :: FilePath -> IO Mapping
loadMapping = JSON.eitherDecodeFileStrict' >=> either error pure

-- | Look up a sprite position by its mapping index.
getMapping :: Mapping -> Int -> ( Int, Int )
getMapping Mapping{..} = fromMaybe fallback . (positions !?)

-- | Given a board of indices, replace each cell with the position it
-- references according to the mapping.
getAllMappings :: Mapping -> Map_ n Int -> Map_ n ( Int, Int )
getAllMappings = fmap . getMapping

-- | A sprite sheet is a large file containing lots of tiles that we can put
-- together to render our scene. The tiles are assumed to be square, and spaced
-- with a regular margin.
data Sprites
  = Sprites
      { _margin :: Int
      , _sheet  :: DynamicImage
      , _size   :: Int
      }
  deriving Eq

-- | Load a sprite sheet with a given tile size and margin. It's assumed that
-- a sprite position @(x, y)@ can be scaled by @size + margin@ to find the
-- pixel-value coordinate of its top-left corner. The bottom right is therefore
-- @(x * (size + margin) + size, y * (size + margin) + size)@.
loadSprites :: FilePath -> Int -> Int -> IO Sprites
loadSprites filepath _size _margin
  = Image.readPng filepath >>= \case
      Right _sheet -> pure Sprites{..}
      Left message -> error message

-- | Get the sprite at a given index from a sprite sheet.
getSprite :: Sprites -> ( Int, Int ) -> DynamicImage
getSprite Sprites{..} ( x, y ) = do
  let mapping :: Pixel px => Image px -> Image px
      mapping image = do
        let getPixel i j = do
              let x' = x * scale + i
                  y' = y * scale + j

              Image.pixelAt image x' y'

        Image.generateImage getPixel _size _size

      scale = _size + _margin

  Image.dynamicPixelMap mapping _sheet

-- | Convert a map of positions to a map of sprites.
getAllSprites :: Sprites -> Map_ n ( Int, Int ) -> Map_ n DynamicImage
getAllSprites = fmap . getSprite
