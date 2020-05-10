{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Example.WFC where

import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Generator
import Generator (Connections)
import qualified Map
import Map (Coordinate, Map)
import Text.Show.Unicode (uprint)

-- | Print a character map to the terminal.
printMap :: Map 2 Char -> IO ()
printMap (Map.toNestedLists -> xs) = do
  putStrLn mempty
  mapM_ uprint (transpose xs)
  putStrLn mempty

-- | Some syntactic sugar for specifying the constraint function.
connect :: (value, value, value, value) -> Connections 4 value
connect (left, right, top, bottom) = Generator.connect @4 left right top bottom

-- | The constraint function: specify the valid neighbours for each possible
-- tile.
maze :: Maybe Char -> Connections 4 (Set Char)
maze = fmap Set.fromList . connect . \case
    Just '─' -> ( left   , right   , noTop , noBottom )
    Just '│' -> ( noLeft , noRight , top   , bottom   )
    Just '┼' -> ( left   , right   , top   , bottom   )
    Just '┌' -> ( noLeft , right   , noTop , bottom   )
    Just '┘' -> ( left   , noRight , noTop , bottom   )
    Just '└' -> ( noLeft , right   , top   , noBottom )
    Just '┐' -> ( left   , noRight , noTop , bottom   )
    Just '┴' -> ( left   , right   , noTop , bottom   )
    Just '┬' -> ( left   , right   , top   , noBottom )
    Just '├' -> ( noLeft , right   , top   , bottom   )
    Just '┤' -> ( left   , noRight , top   , bottom   )
    Just  _  -> ( noLeft , noRight , noTop , noBottom )
    Nothing  -> ( left   , right   , top   , bottom   )
  where
    left     = "─┼┌└┴┬├"
    noLeft   = " │┘┐┤"
    right    = "─┼┐┘┴┬┤"
    noRight  = " │└┌├"
    top      = "│┼┌┐┬├┤"
    noTop    = " ─┘└┴"
    bottom   = "│┼└┘┴├┤"
    noBottom = " ─┐┌┬"

main :: IO ()
main = do
  let dimensions :: Coordinate 2
      dimensions = Map.pos @2 40 20

      initial :: [ Char ]
      initial = " ─│┼┌┘└┐┴┬├┤" 

  Generator.runOne (Generator.collapse dimensions initial maze) >>= \case
    Just result -> printMap result
    Nothing     -> error "Couldn't produce a map!"
