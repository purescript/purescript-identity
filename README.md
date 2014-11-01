# Module Documentation

## Module Data.Identity

### Types

    newtype Identity a where
      Identity :: a -> Identity a


### Type Class Instances

    instance applicativeIdentity :: Applicative Identity

    instance applyIdentity :: Apply Identity

    instance bindIdentity :: Bind Identity

    instance comonadIdentity :: Comonad Identity

    instance eqIdentity :: (Eq a) => Eq (Identity a)

    instance extendIdentity :: Extend Identity

    instance foldableIdentity :: Foldable Identity

    instance functorIdentity :: Functor Identity

    instance monadIdentity :: Monad Identity

    instance ordIdentity :: (Ord a) => Ord (Identity a)

    instance showConst :: (Show a) => Show (Identity a)

    instance traversableIdentity :: Traversable Identity


### Values

    runIdentity :: forall a. Identity a -> a