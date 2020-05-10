{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Map.Coordinate where

import Data.Nat.Extra (Unarise)
import Data.Vect (Vect (..), Make (..))
import qualified GHC.TypeLits as TL
import Prelude hiding (subtract)

-- | For our purposes, an @n@-dimensional coordinate is an @n@-element vector
-- of @Int@ values. Note that it isn't necessarily bounded to a space -
-- coordinates can fall outside our "world".
type Coordinate (n :: TL.Nat)
  = Vect (Unarise n) Int

-- | Add two coordinates (as if they're @n@-dimensional vectors). This allows
-- us to re-frame coordinates in terms of non-origin points.
(.+) :: Vect n Int -> Vect n Int -> Vect n Int
(.+) (x :. xs) (y :. ys) = x + y :. xs .+ ys
(.+)  Nil       Nil      = Nil

infixr 5 .+

-- | Re-export 'Data.Vect.make', but with a 'GHC.TypeLits.Nat' argument to make
-- use of the delicious syntactic sugar.
pos :: forall n k. Make (Unarise n) Int k => k
pos = make @(Unarise n)
