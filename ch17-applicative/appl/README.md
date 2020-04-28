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
### Applicative laws
1. Identity
```
pure id <*> v = v
```
2. Composition  
```
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```
3. Homomorphism
```
pure f <*> pure x = pure (f x)
```

### Checkers
https://github.com/conal/checkers  
Check properties on standard classes and data structures  

