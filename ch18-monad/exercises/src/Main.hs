module Main where

import Control.Monad (ap)
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
