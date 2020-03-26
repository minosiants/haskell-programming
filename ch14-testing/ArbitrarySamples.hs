-- ArbitrarySamples.hs

module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

-- Identety
data Identety a = Identety a deriving (Eq, Show)

identetyGen :: Arbitrary a => Gen (Identety a)
identetyGen = do 
    a <- arbitrary
    return (Identety a)

instance Arbitrary a => 
         Arbitrary (Identety a) where
         arbitrary = identetyGen

identetyGenInt :: Gen (Identety Int)
identetyGenInt = identetyGen

--Product 
data Pair a b = Pair a b deriving (Eq, Show)
pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a , Arbitrary b) => 
      Arbitrary (Pair a b) where
      arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Sum
data Sum a b = 
      First a 
    | Second b deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b) 
sumGen = do
  a <- arbitrary 
  b <- arbitrary
  oneof [return $ First a , return $ Second b]

instance (Arbitrary a, Arbitrary b) => 
    Arbitrary (Sum a b) where
    arbitrary = sumGen

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGen


main :: IO ()
main = do
    sample trivialGen
