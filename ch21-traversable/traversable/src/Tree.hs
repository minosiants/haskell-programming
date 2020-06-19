-- Tree.hs

module Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) =
    Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) =
    foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, pure Empty),
        (2, Leaf <$> arbitrary),
        (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main = do
  let triggerTree :: Tree (Int, Int, [Int])
      triggerTree = undefined
  quickBatch (traversable triggerTree)
