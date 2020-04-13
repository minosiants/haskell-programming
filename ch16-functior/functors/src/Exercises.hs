-- Exercises.hs
{-# LANGUAGE FlexibleInstances #-}

module Exercises where

data Sum a b = 
      First b 
    | Second a deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First b) = First (f b)
  fmap f (Second a) = Second a

data Company a b c =
      DeepBlue a b
    | Something c

instance Functor (Company a b) where  
  fmap _ (DeepBlue a b) = DeepBlue a b
  fmap f (Something c) = Something (f c)

data More b a =
      L a b a
    | R b a b deriving (Eq, Show)

instance Functor (More b ) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b = 
      Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =  K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip (K' (f b))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a  = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f ) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = Dawrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Dawrappa fa ga) = Dawrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a ) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa ( fmap f gb)

data Notorious g o a t = 
  Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = 
    Nil
  | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show, Eq)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = 
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read f' ) = Read (\x -> (f (f' x)))
