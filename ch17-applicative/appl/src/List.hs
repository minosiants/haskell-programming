-- Apl1.hs

module List where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = 
    Nil
  | Cons a (List a) deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
   Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a ) -> List a
concat' = fold append Nil 

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)


flatMap :: ( a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as) 

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) xs ys = flatMap (\f -> (fmap f ys)) xs  


listGen :: Arbitrary a => Gen (List a)
listGen = do 
  l <- listOf arbitrary
  return (foldr(\x res  -> (Cons x res)) Nil l)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
quickBatch $ applicative (undefined :: (List (String, Int, Char)))
