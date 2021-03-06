-- EitherMonad.hs

module EitherMonad where

import Control.Monad (ap)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck(oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = 
      First a 
   |  Second b deriving (Eq, Show)


instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

instance  Applicative (Sum a) where
  pure = Second
  (<*>) = ap 

instance  Monad (Sum a) where
  return  = pure
  (>>=)(First a) _ = First a 
  (>>=)(Second b) f = f b
  
instance (Arbitrary a , Arbitrary b) 
    => Arbitrary (Sum a b) where
      arbitrary = 
          oneof [ (First <$> arbitrary),
                  (Second <$> arbitrary)]
        
instance (Eq a , Eq b) 
  => EqProp (Sum a b) where
    (=-=) = eq


main :: IO ()
main = do 
  quickBatch $ monad (undefined::(Sum (Int,Char,String) (Int, Char, String)))  
