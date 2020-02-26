-- sipher.hs

module Cipher where

import Data.Char



getLetter :: Integer -> String -> Char
getLetter i xs = go xs 1
          where go (x':xs') index
                 | i == index = x'
                 | otherwise = go xs' (index +1) 
          
getIndex :: Char -> String -> Integer
getIndex ch xs = go xs 1
          where go (x':xs') index
                 | ch == x' = index
                 | otherwise = go xs' (index +1)

shift :: Integer -> Integer
shift step
      | mod step 26 == 0 = 26
      | otherwise = mod step 26



encodeLetter :: Integer -> Char -> Char
encodeLetter step ch = 
         getLetter index' letters
           where 
             letters = ['a'.. 'z']
             index' = shift $ step + (getIndex ch letters)

caesar :: Integer -> String -> String
caesar step word = fmap (encodeLetter step) word 
             
unCaesar :: Integer -> String -> String
unCaesar step word = caesar (-1 * step) word  

type Keyword = String
type Word' = String

vigenere :: Keyword -> Word' -> String
vigenere [] [] = ""
vigenere (x:xs) (y:ys) =
     (encodeLetter (getIndex x letters) y) : vigenere xs ys
        where letters = ['a'..'z']
