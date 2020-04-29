-- Validagion.hs

module Validation where

import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck(oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a  =
    Failure e
  | Success a deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Failure e)= Failure e 
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) 
  where 
    pure a = Success a
    (<*>) (Failure e) (Failure e') = Failure $ mappend e e'
    (<*>) (Failure e) _ = Failure e
    (<*>) _ (Failure e) = Failure e
    (<*>) (Success f) (Success a) = Success $ f a

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary e , Arbitrary a) => 
    Arbitrary (Validation e a) where
      arbitrary = 
        oneof [ (Success <$> arbitrary), 
                (Failure <$> arbitrary)]
main :: IO ()
main = do
  quickBatch $ functor (undefined::(Validation ((Sum Int), All, String) (Int, Char, String)))
  quickBatch $ applicative (undefined::(Validation ((Sum Int), All, String) (Int, Char, String)))
        
