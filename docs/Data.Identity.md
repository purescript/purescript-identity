## Module Data.Identity

#### `Identity`

``` purescript
newtype Identity a
  = Identity a
```

##### Instances
``` purescript
instance eqIdentity :: (Eq a) => Eq (Identity a)
instance ordIdentity :: (Ord a) => Ord (Identity a)
instance boundedIdentity :: (Bounded a) => Bounded (Identity a)
instance showIdentity :: (Show a) => Show (Identity a)
instance functorIdentity :: Functor Identity
instance invariantIdentity :: Invariant Identity
instance applyIdentity :: Apply Identity
instance applicativeIdentity :: Applicative Identity
instance bindIdentity :: Bind Identity
instance monadIdentity :: Monad Identity
instance extendIdentity :: Extend Identity
instance comonadIdentity :: Comonad Identity
instance foldableIdentity :: Foldable Identity
instance traversableIdentity :: Traversable Identity
```

#### `runIdentity`

``` purescript
runIdentity :: forall a. Identity a -> a
```


