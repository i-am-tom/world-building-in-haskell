{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Example.Isometric where

import Codec.Picture (PixelRGB8 (..), PixelRGBA8 (..))
import qualified Codec.Picture as Image
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import GHC.Generics (Generic)
import qualified Generator
import Generator (Connections)
import qualified Map
import Map (Coordinate)
import qualified Render
import Render (Tiles (..))
import Text.Printf (printf)

-- | The six types of terrains that meet at the edges of the tiles.
data Terrain
  = Sand | Water | Grass | Road | Dirt | Banked | Sky
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Each edge can have three terrains (e.g. beach on one side, water on the
-- other).
newtype Edge
  = Edge (Terrain, Terrain, Terrain)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Each tile borders 6 others (west, east, north, south, up, down).
newtype Borders
  = Borders (Edge, Edge, Edge, Edge, Edge, Edge)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Configuration for mapping is a hashmap from the tile's image name to the
-- ( North, East, South, West ) edges.
newtype Config
  = Config { toHashMap :: HashMap FilePath Borders }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | There are six sides to a 3D tile.
data Side = North | East | South | West | Up | Down

-- | Look up the connecting tiles in the config.
connections :: Borders -> Config -> Connections 6 (Set FilePath)
connections (Borders (w, e, n, s, u, d)) (Config config)
  = fmap (Set.fromList . get . filterer)
      $ Generator.connect @6 West East North South Up Down
  where
    get :: (Borders -> Bool) -> [ FilePath ]
    get f = HashMap.keys (HashMap.filter f config)

    filterer :: Side -> Borders -> Bool
    filterer side (Borders (w', e', n', s', u', d'))
      = case side of
          North -> n == s'
          East  -> e == w'
          South -> s == n'
          West  -> w == e'
          Up    -> u == d'
          Down  -> d == u'

-- | Given a config, create a constraint function.
makeConstraints :: Config -> Maybe FilePath -> Connections 6 (Set FilePath)
makeConstraints config@(Config hashMap) = \case
    Just filepath | Just borders <- HashMap.lookup filepath hashMap ->
      connections borders config

    _ -> Generator.connect @6 idc idc idc idc idc idc
  where idc = Set.fromList (HashMap.keys hashMap)

main :: IO ()
main = do
  config@(Config hashMap) <-
    Aeson.eitherDecodeFileStrict "iso-mappings.json" >>= \case
      Right success -> pure success
      Left message -> error message

  let dimensions :: Coordinate 3
      dimensions = Map.pos @3 10 10 1

      constraints :: Maybe FilePath -> Connections 6 (Set FilePath)
      constraints = makeConstraints config

      initial :: [ FilePath ]
      initial = HashMap.keys hashMap

  frames <- for [ 1 .. 50 ] \i -> do
    putStrLn (printf "%d / 50" (i :: Int))

    Generator.runOne (Generator.collapse dimensions initial constraints) >>= \case
      Nothing     -> error "Couldn't produce a map!"
      Just result -> Render.isometric (Tiles 100 65 15 "isometric-tiles") id result

  let opaque = \case
        PixelRGBA8 _ _ _ 0 -> PixelRGB8 255 255 255
        PixelRGBA8 r g b _ -> PixelRGB8 r   g   b

      animation = Image.writeGifAnimation "examples/08-isometric.gif"
        30 Image.LoopingForever (map (Image.pixelMap opaque) frames)

  putStrLn "Rendering..."
  case animation of
    Left message -> error message
    Right gif -> gif
