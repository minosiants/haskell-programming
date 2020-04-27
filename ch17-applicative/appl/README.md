# Applicative


```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

### Applicative functors are monoidal functors
```
($) :: (a -> b) -> a -> b
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```


### List applicative
```
Prelude> :set -XTypeApplications
Prelude> :type (<*>) @[]
```
