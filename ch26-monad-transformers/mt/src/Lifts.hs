module Lifts where

import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

class MonadTrans' t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans' (ExceptT e) where
  lift = ExceptT . liftM Right

instance MonadTrans' (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
