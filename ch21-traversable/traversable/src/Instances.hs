--Instances.hs
--
module Instances where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a
  = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap m (Identity a) = m a

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

newtype Constant a b
  = Constant {getConstant :: a}
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

data Optional a
  = Nada
  | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = f a <> (foldMap f l)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a l) =
    Cons <$> f a <*> (traverse f l)

data Three a b c
  = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) (f c)

data Pair a b = Pair a b deriving (Show, Eq)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = fmap (Pair a) (f b)

data Big a b
  = Big a b b
  deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b b1) = Big a (f b) (f b1)

instance Foldable (Big a) where
  foldMap f (Big a b b1) = (f b) <> (f b1)

instance Traversable (Big a) where
  traverse f (Big a b b1) =
    liftA2 (Big a) (f b) (f b1)

data Bigger a b
  = Bigger a b b b
  deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a b b1 b2) =
    Bigger a (f b) (f b1) (f b2)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b1 b2) =
    (f b) <> (f b1) <> (f b2)

instance Traversable (Bigger b) where
  traverse f (Bigger a b b1 b2) =
    liftA3 (Bigger a) (f b) (f b1) (f b2)

-----

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [Yep <$> arbitrary, return Nada]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(3, Cons <$> arbitrary <*> arbitrary), (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

type TI = []

main = do
  let triggerIdentity :: Identity (Int, Int, [Int])
      triggerIdentity = undefined
  quickBatch (traversable triggerIdentity)
  let triggerConstant :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      triggerConstant = undefined
  quickBatch (traversable triggerConstant)
  let triggerOptinal :: Optional (Int, Int, [Int])
      triggerOptinal = undefined
  quickBatch (traversable triggerOptinal)
  let triggerList :: List (Int, Int, [Int])
      triggerList = undefined
  quickBatch (traversable triggerList)
  let triggerThree :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      triggerThree = undefined
  quickBatch (traversable triggerThree)
  let triggerPair :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      triggerPair = undefined
  quickBatch (traversable triggerPair)
  let triggerBig :: Big (Int, Int, [Int]) (Int, Int, [Int])
      triggerBig = undefined
  quickBatch (traversable triggerBig)
  let triggerBigger :: Bigger (Int, Int, [Int]) (Int, Int, [Int])
      triggerBigger = undefined
  quickBatch (traversable triggerBigger)
