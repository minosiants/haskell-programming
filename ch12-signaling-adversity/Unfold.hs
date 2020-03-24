-- Unfold.hs

module Unfold where

myIterate :: (a -> a) -> a -> [a]
myIterate f z = 
      z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
      Just (x, y) -> x : myUnfoldr f y
      Nothing -> myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = 
      myUnfoldr 
          (\b -> Just (b, f b)) z

data BinaryTree a =
          Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) 
          -> a 
          -> BinaryTree b
unfold f z = case f z of
      Just (l, b ,r) -> Node (unfold f l) b (unfold f r)
      Nothing -> Leaf 

treeBuild :: Integer -> BinaryTree Integer
treeBuild i = unfold f 0
    where f a
            | i == a = Nothing
            | otherwise = Just (a+1, a, a+1)

