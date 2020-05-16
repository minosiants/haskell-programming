#Monads
[Control.Monad](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html)

### Laws

#### Identity
```
m >>= return = m
```
```
return x >>= f = f x
```
#### Associativity
```
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```


