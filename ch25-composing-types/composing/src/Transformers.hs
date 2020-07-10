{-# LANGUAGE InstanceSigs #-}

module Transformeres where

import Control.Monad (join)

newtype Identity a
  = Identity {runIdentity :: a}
  deriving (Eq, Show)

newtype IdentityT f a
  = IdentityT {runIdentityT :: f a}
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ f <$> fa

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity fa) (Identity a) = Identity (fa a)

instance
  (Applicative m) =>
  Applicative (IdentityT m)
  where
  pure = IdentityT . pure
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
{-(IdentityT ma) >>= f =
  let aimb = join (fmap runIdentityT (fmap f ma))
   in IdentityT aimb
   -}
{-(IdentityT ma) >>= f =
  let aimb = join (fmap (runIdentityT . f) ma)
   in IdentityT aimb
-}
-- x >>= f = join (fmap f x)
