{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Nat.Extra where

import Data.Nat (Nat (..))
import GHC.TypeLits (type (-))
import qualified GHC.TypeLits as TypeLits

-- | Turn a 'GHC.TypeLits.Nat' into a 'Data.Nat.Nat'. Useful really only to
-- provide a neat API to the users while working internally with a unary
-- representation for convenience (e.g. pattern-matching).
type family Unarise (n :: TypeLits.Nat) :: Nat where
  Unarise 0 = 'Z
  Unarise n = 'S (Unarise (n - 1))

-- | Double a 'Nat' at the type level. We can't call it 'Double', though,
-- because... well, they're already important.
type family Twice (n :: Nat) :: Nat where
  Twice  'Z    = 'Z
  Twice ('S n) = 'S ('S (Twice n))
