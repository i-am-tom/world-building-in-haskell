{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Map.Slice
  ( Slice
  , slice
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Nat (Nat (..))
import Data.Nat.Extra (Unarise)
import Map.Type (Map_ (..))

-- | Take "slices" of a 'Map_' along a given dimension. For example, we might
-- take a @Map 3 Int@ and 'slice' along dimension @2@ (the @z@ dimension), and
-- this would give us a list of the @2@-dimensional maps taken at each point
-- along the @z@ axis.
slice :: forall i d x. Slice (Unarise i) ('S d) => Map_ ('S d) x -> NonEmpty (Map_ d x)
slice = peel . pull @(Unarise i)

-- | Get the contents of the 'Axis' constructor of a 'Map_'.
peel :: Map_ ('S m) x -> NonEmpty (Map_ m x)
peel (Axis xs) = xs

-- | Transpose the two outermost layers of a given 'Map_'.
transpose :: Map_ ('S ('S n)) x -> Map_ ('S ('S n)) x
transpose = Axis . fmap Axis . NonEmpty.transpose . fmap peel . peel

-- | To implement 'slice', we need to be able to reorder the dimensions within
-- a 'Map_'. This function takes a given dimension, and "pulls" it to the
-- outermost layer. For example, if we "pull" the @z@ dimension of a 3D map,
-- this function will take an @x, y, z@ 'Map_' and return a @z, x, y@ 'Map_'.
-- After that, we can peel back the top layer, and we have our slices!
class Slice (choice :: Nat) (dimensions :: Nat) where
  pull :: Map_ dimensions x -> Map_ dimensions x

instance (Slice n ds, d ~ 'S ds, ds ~ 'S dss)
    => Slice ('S n) d where
  pull = transpose . Axis . fmap (pull @n) . peel

instance d ~ 'S n => Slice 'Z d where
  pull = id
