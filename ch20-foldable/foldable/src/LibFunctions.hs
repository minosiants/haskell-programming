-- LibFunctions.hs

module LibFunctions where

import Data.Foldable
import Data.Maybe
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\y a -> x == y || a) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a = getAny . foldMap (Any . (== a))

minimum' ::
  (Foldable t, Ord a) =>
  t a ->
  Maybe a
minimum' =
  foldr
    ( \x a ->
        case (fmap (\y -> if y > x then x else y) a) of
          Just v -> (Just v)
          Nothing -> (Just x)
    )
    Nothing

maximum' ::
  (Foldable t, Ord a) =>
  t a ->
  Maybe a
maximum' =
  foldr
    ( \x a ->
        case (fmap (\y -> if y < x then x else y) a) of
          Just v -> (Just v)
          Nothing -> (Just x)
    )
    Nothing

null' :: (Foldable t) => t a -> Bool
null' ta = (length' ta) == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> a + 1) 0

maximum'' ::
  (Foldable t, Ord a) =>
  t a ->
  Maybe a
maximum'' xs
  | null' xs = Nothing
  | otherwise = Just $ foldr max (head $ toList' xs) xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' ::
  (Foldable t, Monoid m) =>
  (a -> m) ->
  t a ->
  m
foldMap' f = foldr (\x a -> f x <> a) mempty

toList' :: Foldable t => t a -> [a]
toList' = foldr (\x a -> x : a) []


toList'' :: Foldable t => t a -> [a]
toList'' = foldr (:) []
