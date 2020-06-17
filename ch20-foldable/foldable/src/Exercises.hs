-- Exercises.hs

module Exersices where

import Data.Foldable

data Constant a b
  = Constant b
  deriving (Show, Eq)

instance Foldable (Constant a) where
  foldr f b (Constant a) = f a b

data Two a b
  = Two a b
  deriving (Show, Eq)

instance Foldable (Two a) where
  foldMap m (Two a b) = m b

data Three a b c
  = Three a b c
  deriving (Show, Eq)

instance Foldable (Three a b) where
  foldMap m (Three _ _ c) = m c

data Three' a b
  = Three' a b b
  deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap m (Three' _ b b') = m b <> m b'

data Four' a b
  = Four' a b b b
  deriving (Show, Eq)

instance Foldable (Four' a) where
  foldMap m (Four' _ b1 b2 b3) = m b1 <> m b2 <> m b3

filterF ::
  (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) ->
  t a ->
  f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
