-- Exercises.hs

module Exercises where

newtype State s a
  = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (ss, a) = g s
     in (ss, f a)

instance Applicative (State s) where
  pure a = State $ \s -> (s, a)
  (State f) <*> (State g) = State $ \s ->
    let (s1, ff) = f s
        (s2, a) = g s1
     in (s2, ff a)

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s ->
    let (s1, a) = f s
     in runState (g a) s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const (s, ())

exec :: State s a -> s -> s
exec (State sa) s = fst (sa s)

eval :: State s a -> s -> a
eval (State sa) s = snd (sa s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f
