--Moi.hs

module Moi where

newtype Moi s a
  = Moi {runMoi :: s -> (s, a)}

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s ->
    let (ss, a) = g s
     in (ss, f a)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (s, a)
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (s1, ff) = f s
        (s2, a) = g s1
     in (s2, ff a)

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s ->
    let (s1, a) = f s
     in runMoi (g a) s1
