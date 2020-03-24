-- fold.hs

module Fold where

import Data.Time

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = 
    foldr (\x b -> f x || b) False xs




data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
   [ DbDate (UTCTime
             (fromGregorian 1915 5 1)
           (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello World !"
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))

    ]
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr (\x res -> 
                         case x of
                           DbDate t -> t : res
                           DbNumber _ -> res
                           DbString _ -> res)  [] db 
                          


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr (\x res -> 
                         case x of
                           DbDate _ ->  res
                           DbNumber x -> x : res
                           DbString _ -> res)  [] db 


mostResent :: [DatabaseItem] -> UTCTime
mostResent db = head $ foldr (\x res -> 
                         case x of
                           DbDate t -> 
                              case res of 
                                 [] -> [t]
                                 (x':xs) ->   
                                    if t > x' then [t] else res
                           DbNumber _ -> res
                           DbString _ -> res)  [] db 
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (\x res -> 
                         case x of
                           DbDate _ ->  res
                           DbNumber x -> x + res
                           DbString _ -> res)  0 db 

avgDb :: [DatabaseItem] -> Double
avgDb db =  s / n
   where 
      nums = filterDbNumber db
      s = fromIntegral $ sum nums
      n = fromIntegral $ length nums


svs ::  [Char] -> [Char] -> [(Char, Char, Char)]
svs stops vowels = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]


svs' ::  [Char] -> [Char] -> [(Char, Char, Char)]
svs' stops vowels = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

sentences :: [String] -> [String] -> [(String, String, String)]
sentences nouns verbs = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns]

seekritFunc x = 
     (fromIntegral $ (sum (map length (words x)))) / 
         (fromIntegral $ (length (words x)))



myOr :: [Bool] -> Bool
myOr = foldr 
          (\a b -> 
               if a == True 
               then True
               else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (&&) False 


myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr 
            (\a b ->
                f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr 
            (\a b ->
                x == a || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

myReverse :: [a] -> [a]
myReverse = foldr
               (\a b ->
                   b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr
            (\a b ->
                f a : b)[]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr
               (\a b ->
                   if (f a)
                   then a : b
                   else b) []
squish :: [[a]] -> [a]
squish = foldr
           (\a b ->
              a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr
                 (\a b ->
                     (f a) ++ b) [] 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x) 


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr
                  (\a b ->
                    if  f a b == GT
                    then a
                    else b) x xs
