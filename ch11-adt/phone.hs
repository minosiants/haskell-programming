-- phone,hs
module Phone where

import Data.Char
import Data.List
import Data.Maybe
import Data.String 

type Digit = Char
type Presses = Int

type Symbols = String
data Key = Key Digit Symbols deriving (Eq, Show)
data DaPhone = DaPhone [Key] deriving (Eq, Show)

daPhone = DaPhone[
   Key '1' "1"
 , Key '2' "abc2"
 , Key '3' "def3"
 , Key '4' "ghi4"
 , Key '5' "jkl5"
 , Key '6' "mno6"
 , Key '7' "pqrs7"
 , Key '8' "tuv8"
 , Key '9' "wxyz9"
 , Key '*' "^*"
 , Key '0' "+_0"
 , Key '#' ".,"
 ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

findKey::Char -> [Key] -> Key
findKey ch =
     head . foldr (\k@(Key d s  ) b -> 
        if (any (== ch) s)
        then [k] 
        else b) []

toReverseTap :: Char -> Key -> (Digit, Presses)
toReverseTap ch (Key d s) = (d, presses)
      where 
         presses = case findIndex (==ch) s of
                         Just i -> i+1
                         Nothing -> 0


reverseTap :: DaPhone -> Char -> [(Digit, Presses)]
reverseTap da@(DaPhone keys)  ch
 | isUpper ch  == True = [('*', 1)] ++ reverseTap da (toLower ch)
 | ch == ' ' = reverseTap da '_'
 | otherwise = toReverseTap ch (findKey ch keys) : []

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = 
      foldr (\ch b -> (reverseTap daPhone ch) ++ b) [] 

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> b + snd a) 0


fingerTaps' :: [(Digit, Presses)] -> Presses
fingerTaps' = sum . map snd 


mostPopularLetter :: String -> Char
mostPopularLetter str = ( head . head . groupBy (==) . sort) str

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat 

coolestWord :: [String] -> String
coolestWord = 
    head . head . sortBy (\a b -> compare (length b)  (length a)) . groupBy (==) . sort.concat . fmap words
