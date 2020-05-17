module Main where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a ) where
  (=-=) = eq


----

data BahEither a b =
      PLeft a
   |  PRight b deriving (Show, Eq)

instance Functor ( BahEither a) where
  fmap _ (PLeft a) = PLeft a
  fmap f (PRight b) = PRight (f b)

instance Applicative (BahEither a) where
  pure = PRight
  (<*>) = ap

instance Monad (BahEither a) where
  return = pure
  (>>=) (PLeft a) _ = PLeft a
  (>>=) (PRight a) f = f a  

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (BahEither a b) where
    arbitrary = oneof[ (PLeft <$> arbitrary),
                       (PRight <$> arbitrary)]

instance (Eq a, Eq b) => 
  EqProp ( BahEither a b) where
    (=-=) = eq


--
newtype Identity a = 
  Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
  pure = Identity
  (<*>) = ap

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => 
  Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where 
  (=-=) = eq

--

data List a = 
     Nil
   | Cons a (List a) deriving (Eq, Show)

append:: List a -> List a -> List a
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ append xs ys
  

instance Functor List where
  fmap _ Nil =  Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)  

instance Applicative List where
  pure a = Cons a Nil
  (<*>) = ap

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f  = f a
  (>>=) (Cons a xs) f = append (f a) (xs >>= f)  
    
toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(3, Cons <$> arbitrary <*> arbitrary), (1, return Nil)]


instance Eq a => EqProp (List a) where
  (=-=) = eq
  

--

j :: Monad m => m ( m a ) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip  ap

meh :: Monad m 
  => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = do
    v <- f x
    vx <- meh xs f
    return $ v:vx
  
meh' :: Monad m 
  => [a] -> (a -> m b) -> m [b]
meh'  = flip  mapM

flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma id  

main :: IO ()
main = do
  let triggerNope :: Nope (Int, String, Int)
      triggerNope = undefined

  quickBatch $ functor triggerNope
  quickBatch $ applicative triggerNope
  quickBatch $ monad triggerNope
  
  let triggerBahEither :: BahEither (Int, String, Int) (Int, String, Int)
      triggerBahEither = undefined

  quickBatch $ functor triggerBahEither
  quickBatch $ applicative triggerBahEither
  quickBatch $ monad triggerBahEither

  let triggerIdentity :: Identity (Int, String, Int)
      triggerIdentity = undefined

  quickBatch $ functor triggerIdentity
  quickBatch $ applicative triggerIdentity
  quickBatch $ monad triggerIdentity

  let triggerList :: List (Int, String, Int)
      triggerList = undefined

  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList
