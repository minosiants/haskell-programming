-- MonoidExercises.hs

module MonoidExercises where 

import Data.Monoid
import Test.QuickCheck

import SemigroupExercises hiding (runQuickCheck)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = mempty <>  x == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = x <> mempty == x

instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty 
  mappend = (<>)

instance (Monoid a, Monoid b) => 
      Monoid (Two a b) where
        mempty = Two mempty mempty 
        mappend = (<>)

instance Monoid BoolConj where
      mempty = BoolConj True
      mappend = (<>)

instance Monoid BoolDisj where
        mempty = BoolDisj False
        mappend = (<>)

runQuckCheck :: IO ()
runQuckCheck = do
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (monoidLeftIdentity :: (Identity (Sum Int)) -> Bool)
    quickCheck (monoidRightIdentity :: (Identity (Sum Int)) -> Bool)
    quickCheck (monoidLeftIdentity :: (Two String String) -> Bool)
    quickCheck (monoidRightIdentity :: (Two String String) -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
