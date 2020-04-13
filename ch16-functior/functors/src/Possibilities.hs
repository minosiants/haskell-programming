{-# LANGUAGE RankNTypes #-}-- Possibilities.hs

module Possiblities where

data Possibly a = 
      LolNope
   |  Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b =
    First a
  | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f 
    => Functor (Wrap f)  where
      fmap f (Wrap fa) = Wrap (fmap f fa)

-- Natural transformation

type Nat f g = forall a . f a  -> g a

--maybeToList :: Nat Maybe []
--maybeToList Nothing = []
--maybeToList Just a = [a]
