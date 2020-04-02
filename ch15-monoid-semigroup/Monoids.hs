-- Monoids.hs

module Monoids where

import Data.Monoid
import Data.Maybe
import Test.QuickCheck

-- First -  First which is not Nothing
mfirst :: IO ()
mfirst = 
  putStrLn . show . getFirst $ 
        First (Just 1) `mappend` First (Just 2)
-- Lasr - Last wich is not Nothing
mlast :: IO ()
mlast = 
  putStrLn . show . getLast $
        Last (Just 1) `mappend` Last(Just 2)

data Optional a = 
      Nada
    | Only a deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where
    (<>) (Only a) (Only b)= Only (  a <> b)
    (<>) (Only a) _ = Only a
    (<>) _ (Only b) = Only b
    (<>) _ _ = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

newtype First' a = 
      First' {getFirst'::Optional a} deriving (Show, Eq)

instance Semigroup (First' a)  where
    (<>) f f'= 
          case  (getFirst' f) of 
              Only a -> First' (Only a)
              _ -> First' (getFirst' f')

instance Monoid (First' a) where 
    mempty = First' Nada


firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
    v <- arbitrary
    frequency[ (1, return $ First' Nada), 
              (2, return $ First' (Only v))]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = firstGen


firstMappend :: First' a
             -> First' a
             -> First' a

firstMappend = mappend

type FirstMappend = 
            First' String
        ->  First' String
        -> First' String
        -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


main :: IO ()
main = do
    quickCheck (monoidAssoc::FirstMappend)
    quickCheck (monoidLeftIdentity::FstId)
    quickCheck (monoidRightIdentity::FstId)
