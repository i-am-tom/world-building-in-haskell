module Map
  ( Coordinate, pos, (.+)
  , Map, Map_ (..), (?!)
  , Nested (..)
  , Slice, slice
  , Vect (..)

  , create
  , dimensions
  , kernels
  ) where

import Data.Vect (Vect (..))
import Map.Coordinate (Coordinate, (.+), pos)
import Map.Kernel (kernels)
import Map.Slice (Slice, slice)
import Map.Type ((?!), Map, Map_ (..), Nested (..), create, dimensions)
import Prelude hiding (iterate)
