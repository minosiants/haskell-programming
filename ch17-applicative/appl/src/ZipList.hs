-- ZipList.hs

module ZipList where


import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype ZipList' a = 
  ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys' 
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList'  l) = ys
                in take 3000 l

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs 

append :: a -> ZipList' a -> ZipList' a
append a (ZipList' xs) = (ZipList' (a:xs))

instance Applicative ZipList' where
  pure a = ZipList' (repeat a)
  (<*>) (ZipList' []) _ = ZipList' []
  (<*>) _ (ZipList' [])  = ZipList' []
  (<*>) (ZipList' (f:xs)) (ZipList' (y:ys)) =
       append (f y) ((ZipList' xs) <*> (ZipList' ys))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    l <- listOf arbitrary
    return (ZipList' l)

main :: IO ()
main = do
quickBatch $ applicative (undefined :: (ZipList' (String, Int, Char)))
