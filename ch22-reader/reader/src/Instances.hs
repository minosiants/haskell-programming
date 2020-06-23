{-# LANGUAGE InstanceSigs #-}

-- FunApplicative.hs
--

module FunApplicative where

newtype HumanName
  = HumanName String
  deriving (Show, Eq)

newtype DogName
  = DogName String
  deriving (Show, Eq)

newtype Address
  = Address String
  deriving (Show, Eq)

data Person
  = Person
      { humanName :: HumanName,
        dogName :: DogName,
        address :: Address
      }
  deriving (Show, Eq)

data Dog
  = Dog
      { dogsName :: DogName,
        dogAddress :: Address
      }
  deriving (Show, Eq)

pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

--  without Reader

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

myLiftA2 ::
  Applicative f =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
myLiftA2 f fa fb = f <$> fa <*> fb

newtype Reader r a = Reader {runReader :: r -> a}

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a
  (<*>) ::
    Reader r (a -> b) ->
    Reader r a ->
    Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) ::
    Reader r a ->
    (a -> Reader r b) ->
    Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

 
