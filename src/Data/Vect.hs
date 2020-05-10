{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Vect where

import Data.Foldable (sequenceA_)
import Data.Kind (Type)
import Data.Monoid (Ap (..))
import Data.Nat (Nat (..))
import Prelude hiding (zip, zipWith)

-- | Length-indexed list type. A @Vect 2 Int@ contains __exactly__ two @Int@
-- values.
data Vect (n :: Nat) (x :: Type) where
  Nil  :: Vect 'Z x
  (:.) :: x -> Vect n x -> Vect ('S n) x

infixr 4 :.

deriving instance Eq x => Eq (Vect n x)
deriving instance Functor (Vect n)
deriving instance Foldable (Vect n)
deriving instance Traversable (Vect n)

deriving via (Ap (Vect n) x)
  instance (Applicative (Vect n), Num x)
    => Num (Vect n x)

deriving via (Ap (Vect n) x)
  instance (Applicative (Vect n), Semigroup x)
    => Semigroup (Vect n x)

deriving via (Ap (Vect n) x)
  instance (Applicative (Vect n), Monoid x)
    => Monoid (Vect n x)

instance Show x => Show (Vect n x) where
  show = show . toList

instance Applicative (Vect 'Z) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (Vect n)
    => Applicative (Vect ('S n)) where
  pure x = x :. pure x
  (f :. fs) <*> (x :. xs) = f x :. (fs <*> xs)

-- | Construct a vector without the constructor noise. Given a number of
-- arguments, this constructs a function to populate a vector.
--
-- >>> make @'Z
-- Vect 'Z x
--
-- >>> make @('S 'Z)
-- x -> Vect ('S 'Z) x
--
-- >>> make @('S ('S 'Z))
-- x -> x -> Vect ('S ('S 'Z)) x
class Make (count :: Nat) (value :: Type) (signature :: Type)
    | count value -> signature
    , signature -> count value where
  make :: signature

instance Make_ n n x k => Make n x k where
  make = make_ @n id

class Make_ (todo :: Nat) (total :: Nat) (value :: Type) (output :: Type)
    | todo total value -> output
    , output -> value total where
  make_ :: (Vect todo value -> Vect total value) -> output

instance k ~ Vect n x => Make_ 'Z n x k where
  make_ f = f Nil

instance (Make_ i n x ks, k ~ (x -> ks)) => Make_ ('S i) n x k where
  make_ f x = make_ @i \xs -> f (x :. xs)

-- | Convert a 'Vect' to a list, "forgetting" the number of elements.
toList :: Vect n x -> [ x ]
toList = \case
  x :. xs -> x : toList xs
  Nil     -> []

-- | Zip two vectors together. These vectors must be the same length.
zip :: Vect n x -> Vect n y -> Vect n (x, y)
zip (x :. xs) (y :. ys) = (x, y) :. zip xs ys
zip  Nil       Nil      = Nil

-- | Zip two equal-length vectors together with a given function.
zipWith :: (x -> y -> z) -> Vect n x -> Vect n y -> Vect n z
zipWith f (x :. xs) (y :. ys) = f x y :. zipWith f xs ys
zipWith _  Nil       Nil      = Nil

-- | Zip two equal-length vectors together using a given effectful function.
zipWithA :: Applicative f => (x -> y -> f z) -> Vect n x -> Vect n y -> f (Vect n z)
zipWithA f xs ys = sequenceA (zipWith f xs ys)

-- | Zip two equal-length vectors together using a given effectful function,
-- and discard the result.
zipWithA_ :: Applicative f => (x -> y -> f z) -> Vect n x -> Vect n y -> f ()
zipWithA_ f xs ys = sequenceA_ (zipWith f xs ys)
