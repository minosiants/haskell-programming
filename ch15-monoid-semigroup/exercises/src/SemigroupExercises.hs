--SemigroupExercises.hs

module SemigroupExercises where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ =  Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssouc :: (Eq m , Semigroup m) =>
      m -> m -> m -> Bool
semigroupAssouc a b c = (a <> (b <> c)) == ((a<>b) <> c)


type TrivAssouc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssouc  = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool


data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
      Semigroup (Two a b) where
        (Two x y) <> (Two x' y') = Two (x<>x') (y <> y')

instance (Arbitrary a, Arbitrary b) => 
      Arbitrary (Two a b) where
        arbitrary = do
          x <- arbitrary
          y <- arbitrary
          return (Two x y)

type TwoAssouc = Two (Sum Int) String -> Two (Sum Int) String -> Two (Sum Int) String -> Bool

data Tree a b c = Tree a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Tree a b c) where 
        (Tree x y z) <> (Tree x' y' z') =
            Tree (x<>x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Tree a b c) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Tree x y z)

type TreeAssouc = Tree All (Product Int) String 
              ->  Tree All (Product Int) String 
              ->  Tree All (Product Int) String 
              -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
        (Four x y z r) <> (Four x' y' z' r') = 
            Four (x <> x') (y <> y') (z <> z') ( r <> r')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
    => Arbitrary (Four a b c d) where
        arbitrary = do
          x <- arbitrary
          y <- arbitrary
          z <- arbitrary
          r <- arbitrary
          return ( Four x y z r)
type FourItem = Four Any (Product Int) String (Sum Int)
type FourAssouc = FourItem -> FourItem -> FourItem -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
      BoolConj x <> BoolConj x' = BoolConj (x && x')

instance Arbitrary BoolConj where
      arbitrary = do
        x <- arbitrary
        return $ BoolConj x

type BoolConjAssouc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where 
    BoolDisj x <> BoolDisj x' = BoolDisj (x || x')

instance Arbitrary BoolDisj where 
    arbitrary = do
      x <- arbitrary
      return $ BoolDisj x

type BoolDisjAssouc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


data Or a b = 
      Fst a
    | Snd b deriving (Show, Eq)


instance  Semigroup (Or a b) where
      Fst x <> Fst x' = Fst x'
      Fst x <> Snd x' = Snd x'
      Snd x <> _ = Snd x

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Or a b) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        oneof[(return $ Fst x), (return $ Snd y)]

type OrItem = Or Int String        
type OrAssouc = OrItem -> OrItem -> OrItem -> Bool

newtype Combine a b = 
    Combine {unCombine :: (a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
    Combine {unCombine = f} <> Combine {unCombine = g} = Combine (f <> g)


type CombineItem = Combine Int (Sum Int) 
type CombineAssouc = CombineItem -> CombineItem -> CombineItem -> Bool

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
    (Success' b) <> (Success' b') = Success' $ b <> b'
    (Failure' a) <> (Success' _) = Failure' a
    (Success' _) <> (Failure' a) = Failure' a
    (Failure' a) <> (Failure' a') = Failure' $ a <> a'


runQuickCheck :: IO ()
runQuickCheck = do
    quickCheck (semigroupAssouc :: TrivAssouc)
    quickCheck (semigroupAssouc :: IdentityAssouc)
    quickCheck (semigroupAssouc :: TwoAssouc)
    quickCheck (semigroupAssouc :: TreeAssouc)
    quickCheck (semigroupAssouc :: FourAssouc)
    quickCheck (semigroupAssouc :: BoolConjAssouc)
    quickCheck (semigroupAssouc :: BoolDisjAssouc)
    quickCheck (semigroupAssouc :: OrAssouc)
