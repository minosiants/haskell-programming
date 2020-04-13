-- Functors.hs

module Functors where

import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

data Or a b = 
      First a 
    | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => 
    f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
        (a -> b) 
    ->  (b -> c)
    ->  f a
    -> Bool
functorCompose f g x = 
    ( fmap g (fmap f x )) == (fmap (g . f)  x)   


newtype Identity' a = Identity' a deriving (Eq, Show)

instance Functor Identity' where 
  fmap f (Identity' x) = Identity' (f x)   

instance Arbitrary a => 
    Arbitrary (Identity' a) where 
        arbitrary = do
          x <- arbitrary
          return (Identity' x)
    
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair  where
  fmap f (Pair x y ) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return (Pair x x)


instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three x y z


data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three' x y z

data Four a b c d = Four a b c d deriving ( Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z u) = Four x y z (f u)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      u <- arbitrary
      return $ Four x y z u


data Four' a b = Four' a a a b deriving ( Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z u) = Four' x  y  z (f u)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Four' a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      u <- arbitrary
      return $ Four' x y z u

runQuickCheck :: IO()
runQuickCheck = do
    quickCheck (functorIdentity :: Identity' Int -> Bool)
    quickCheck (functorCompose  (+1)(*8):: Identity' Int -> Bool)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Pair Int -> Bool)
    quickCheck (functorIdentity :: Two Int Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Two Int Int -> Bool)
    quickCheck (functorIdentity :: Three Int Int Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Three Int Int Int -> Bool)
    quickCheck (functorIdentity :: Three' Int Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Three' Int Int -> Bool)
    quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Four Int Int Int Int -> Bool)
    quickCheck (functorIdentity :: Four' Int Int -> Bool)
    quickCheck (functorCompose  (+2) (*2) :: Four' Int Int -> Bool)
