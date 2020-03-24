--ch11ex.hs

module Ch1ex where
import Data.Char

isSubseqOf :: (Eq a) 
   => [a]
   -> [a]
   -> Bool

isSubseqOf [] [] = True
isSubseqOf [] ys = True
isSubseqOf xs [] = False
isSubseqOf sub@(x:xs) (y:ys)
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf sub ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords  = fmap capitalize . words
   where capitalize w@(x:xs) = (toUpper x:xs, w)
  
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x:xs 

capitalizeParagraph :: String -> String
capitalizeParagraph   = 
   unwords . cap . words . capitalizeWord 
      where 
         cap [] = []
         cap (x:[]) = [x]
         cap (x:y:xs)
           | last x == '.' = 
             x : cap (capitalizeWord y:xs)
           | otherwise = x:cap (y:xs)    
           
