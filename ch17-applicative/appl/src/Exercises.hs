-- Exersices.hs

module Exersices where
  
import Control.Applicative (liftA3)
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck(oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a  = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair fx fy) (Pair x y) = 
        Pair (fx x) (fy y)

instance Arbitrary a => Arbitrary (Pair a ) where
  arbitrary = (\x -> (Pair x x)) <$> arbitrary 

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

--
--
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative ( Two a) where
  pure b = Two mempty b
  (<*>) (Two a b) (Two a' b') = 
      Two (a <> a') (b b') 

instance (Arbitrary a, Arbitrary b) 
  => Arbitrary (Two a b) where
    arbitrary = do 
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq 

--
--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) 
  => Applicative (Three a b) where
    pure a = (Three mempty mempty a)
    (<*>) (Three a b c) (Three a' b' c') =
        Three (a <> a') (b <> b') (c c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
  => Arbitrary (Three a b c) where
    arbitrary= do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

instance (Eq a, Eq b,  Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--
--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' a f f') (Three' a' b b')  
    = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) 
  => Arbitrary (Three' a b) where
    arbitrary = do
      a <-arbitrary
      b <-arbitrary
      c <-arbitrary
      return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--
--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four  a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
    Applicative (Four a b c) where
      pure v = Four mempty mempty mempty v
      (<*>) (Four a b c f) (Four a' b' c' d) =
          Four (a <> a') (b <> b') (c <>c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
    => Arbitrary (Four a b c d) where
      arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a , Eq b, Eq c, Eq d) 
  => EqProp (Four a b c d) where
    (=-=) = eq


--
--

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure v = Four' mempty mempty mempty v
  (<*>) (Four' x y z f) (Four' x' y' z' v) =
    Four' (x <> x') (y <> y') (z <> z') (f v)


instance (Arbitrary a, Arbitrary b) 
  => Arbitrary (Four' a b) where
    arbitrary = do 
      a <- arbitrary
      a' <- arbitrary
      a'' <- arbitrary
      b <- arbitrary
      return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


--
--

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = 
    liftA3 (\x  y  z -> (x, y, z)) xs ys zs






main :: IO ()
main = do
  quickBatch $ applicative (undefined::Pair(Int, Char, String))  
  quickBatch $ applicative (undefined::Two(Sum Int, All, String)
                                          (Int, Char, String))  
  quickBatch $ applicative (undefined::Three(Sum Int, All, String)
                                             (Sum Int, All, String)
                                             (Int, Char, String))  
  quickBatch $ applicative (undefined::Three'(Sum Int, All, String)
                                             (Int, Char, String))  
  quickBatch $ applicative (undefined::Four(Sum Int, All, String)
                                           (Sum Int, All, String)
                                           (Sum Int, All, String)
                                            (Int, Char, String))  
  quickBatch $ applicative (undefined::Four'(Sum Int, All, String)
                                            (Int, Char, String))  
