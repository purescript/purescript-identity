module Data.Identity where

import Prelude

import Control.Comonad (Comonad)
import Control.Extend (Extend)
import Data.Foldable (Foldable)
import Data.Functor.Invariant (Invariant, imapF)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable)

newtype Identity a = Identity a

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

instance eqIdentity :: (Eq a) => Eq (Identity a) where
  eq (Identity x) (Identity y) = x == y

instance ordIdentity :: (Ord a) => Ord (Identity a) where
  compare (Identity x) (Identity y) = compare x y

instance boundedIdentity :: (Bounded a) => Bounded (Identity a) where
  top = Identity top
  bottom = Identity bottom

instance boundedOrdIdentity :: (BoundedOrd a) => BoundedOrd (Identity a)

instance booleanAlgebraIdentity :: (BooleanAlgebra a) => BooleanAlgebra (Identity a) where
  conj (Identity x) (Identity y) = Identity (conj x y)
  disj (Identity x) (Identity y) = Identity (disj x y)
  not (Identity x) = Identity (not x)

instance semigroupIdenity :: (Semigroup a) => Semigroup (Identity a) where
  append (Identity x) (Identity y) = Identity (x <> y)

instance monoidIdentity :: (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance semiringIdentity :: (Semiring a) => Semiring (Identity a) where
  add (Identity x) (Identity y) = Identity (x + y)
  zero = Identity zero
  mul (Identity x) (Identity y) = Identity (x * y)
  one = Identity one

instance moduloSemiringIdentity :: (ModuloSemiring a) => ModuloSemiring (Identity a) where
  mod (Identity x) (Identity y) = Identity (x `mod` y)
  div (Identity x) (Identity y) = Identity (x / y)

instance ringIdentity :: (Ring a) => Ring (Identity a) where
  sub (Identity x) (Identity y) = Identity (x - y)

instance divisionRingIdentity :: (DivisionRing a) => DivisionRing (Identity a)

instance numIdentity :: (Num a) => Num (Identity a)

instance showIdentity :: (Show a) => Show (Identity a) where
  show (Identity x) = "Identity (" ++ show x ++ ")"

instance functorIdentity :: Functor Identity where
  map f (Identity x) = Identity (f x)

instance invariantIdentity :: Invariant Identity where
  imap = imapF

instance applyIdentity :: Apply Identity where
  apply (Identity f) (Identity x) = Identity (f x)

instance applicativeIdentity :: Applicative Identity where
  pure = Identity

instance bindIdentity :: Bind Identity where
  bind (Identity m) f = f m

instance monadIdentity :: Monad Identity

instance extendIdentity :: Extend Identity where
  extend f m = Identity (f m)

instance comonadIdentity :: Comonad Identity where
  extract (Identity x) = x

instance foldableIdentity :: Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance traversableIdentity :: Traversable Identity where
  traverse f (Identity x) = Identity <$> f x
  sequence (Identity x) = Identity <$> x
