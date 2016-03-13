module Data.Identity where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Monad (class Monad)

import Data.BooleanAlgebra (class BooleanAlgebra, not, (||), (&&))
import Data.Bounded (class Bounded, bottom, top)
import Data.BoundedOrd (class BoundedOrd)
import Data.DivisionRing (class DivisionRing)
import Data.Eq (class Eq, (==))
import Data.Foldable (class Foldable)
import Data.Functor (class Functor, (<$>))
import Data.Functor.Invariant (class Invariant, imapF)
import Data.ModuloSemiring (class ModuloSemiring, mod, (/))
import Data.Monoid (class Monoid, mempty)
import Data.Num (class Num)
import Data.Ord (class Ord, compare)
import Data.Ring (class Ring, (-))
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring, one, zero, (+), (*))
import Data.Show (class Show, show)
import Data.Traversable (class Traversable)

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
  conj (Identity x) (Identity y) = Identity (x && y)
  disj (Identity x) (Identity y) = Identity (x || y)
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
  show (Identity x) = "(Identity " <> show x <> ")"

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
