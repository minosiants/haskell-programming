{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a
  = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT fs) =
    StateT $ \s -> fmap f' (fs s)
    where
      f' (a, s) = (f a, s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: (StateT s m (a -> b)) -> (StateT s m a) -> (StateT s m b)
  (StateT famb) <*> (StateT fa) =
    StateT $ \s -> do
      (f, s') <- (famb s)
      (a, s'') <- (fa s')
      return $ ((f a), s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> (StateT s m b)
  (StateT ma) >>= f = StateT $ \s -> do
    (a, s') <- ma s
    runStateT (f a) s'
