module Data.Identity where

import Control.Comonad (Comonad, extract)
import Control.Extend (Extend, (<<=))
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Functor.Invariant (Invariant, imapF)
import Data.Traversable (Traversable, traverse, sequence)
import Data.Monoid (Monoid, mempty)

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

instance showIdentity :: (Show a) => Show (Identity a) where
  show (Identity x) = "Identity (" ++ show x ++ ")"

instance semiringIdentity :: (Semiring a) => Semiring (Identity a) where
  (+) = liftM2 (+)
  zero = Identity zero
  (*) = liftM2 (*)
  one = Identity one

instance moduloSemiringIdentity :: (ModuloSemiring a) => ModuloSemiring (Identity a) where
  mod = liftM2 mod
  (/) = liftM2 (/)

instance ringIdentity :: (Ring a) => Ring (Identity a) where
  (-) = liftM2 (-)

instance latticeIdentity :: (Lattice a) => Lattice (Identity a) where
  sup = liftM2 sup
  inf = liftM2 inf

instance semigroupIdenity :: (Semigroup a) => Semigroup (Identity a) where
  append = liftM2 append

instance monoidIdentity :: (Monoid a) => Monoid (Identity a) where
  mempty = pure mempty

instance boundedLatticeIdentity :: (BoundedLattice a) => BoundedLattice (Identity a)

instance complementedLatticeIdentity :: (ComplementedLattice a) => ComplementedLattice (Identity a) where
  not x = not <$> x

instance distributiveLatticeIdentity :: (DistributiveLattice a) => DistributiveLattice (Identity a)

instance booleanAlgebraIdentity :: (BooleanAlgebra a) => BooleanAlgebra (Identity a)

instance divisionRightIdentity :: (DivisionRing a) => DivisionRing (Identity a)

instance numIdentity :: (Num a) => Num (Identity a)

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

liftM2 :: forall a m. (Monad m) => (a -> a -> a) -> m a -> m a -> m a
liftM2 f ma ma' = f <$> ma <*> ma'
