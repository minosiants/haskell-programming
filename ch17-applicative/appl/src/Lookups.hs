-- Lookups.hs

module Lookups where 

import Control.Applicative
import Data.List (elemIndex)

added :: Maybe Integer
added =
  pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5 , 6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = pure (,) <*>  y <*> z

------------------

xx :: Maybe Int
xx = elemIndex 3 [1, 2, 3, 4, 5]

yy :: Maybe Int
yy = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> xx <*> yy

-----------------

xs = [1, 2, 3]
ys = [4, 5, 6]

xxx :: Maybe Integer
xxx = lookup 3 $ zip xs ys


yyy :: Maybe Integer
yyy = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$>  ( pure (,) <*> xxx <*> yyy)


