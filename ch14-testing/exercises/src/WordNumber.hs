-- wordNumber.hs

module WordNumber where

import Data.List 

digitToWord :: Int -> String
digitToWord (x)= case x  of
   1 -> "one"
   2 -> "two"
   3 -> "three"
   4 -> "four"
   5 -> "five"
   6 -> "six"
   7 -> "seven"
   8 -> "eight"
   9 -> "nine"
   0 -> "zero"

digits :: Int -> [Int]
digits (x) = go x []
   where go n acc 
           | n > 9 = go (div n 10) ((mod n 10) : acc )
           | otherwise = n : acc 

wordNumber :: Int -> String
wordNumber (n) =  intercalate "-" (map digitToWord(digits n))



