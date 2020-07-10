# composing


### review composition of `runIdentityT . f` in `prelude`

```haskell
Prelude> :{
*Main| let f :: (a -> IdentityT m b)
*Main| f = undefined
*Main| :}
Prelude> :t f
f :: a -> IdentityT m b
Prelude> :t runIdentityT
runIdentityT :: IdentityT f a -> f a
Prelude> :t (runIdentityT . f)
(runIdentityT . f) :: a1 -> f a
```
