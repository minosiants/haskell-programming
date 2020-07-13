{-# LANGUAGE InstanceSigs #-}

--MaybeT.hs

module MaybeT where

import Data.Functor.Identity

newtype MaybeT m a
  = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

--Applicative steps in details

innerMost ::
  [Maybe (Identity (a -> b))] ->
  [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' ::
  [Maybe (Identity a -> Identity b)] ->
  [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' ::
  [Maybe (Identity a) -> Maybe (Identity b)] ->
  [Maybe (Identity a)] ->
  [Maybe (Identity b)]
final' = (<*>)

lmiApply ::
  [Maybe (Identity (a -> b))] ->
  [Maybe (Identity a)] ->
  [Maybe (Identity b)]
lmiApply f x = final' (second' (innerMost f)) x

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (<*>) :: (MaybeT m (a -> b)) -> (MaybeT m a) -> (MaybeT m b)
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

--(MaybeT fab) <*> (MaybeT mma) = MaybeT $ (fmap (<*>) fab) <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: (MaybeT m a) -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)
