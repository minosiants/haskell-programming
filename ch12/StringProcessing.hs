-- StringProcessing.hs

module StringProcessing where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe txt = Just txt

replaceThe :: String -> String
replaceThe  = unwords . ( map  replace) . words
        where 
         replace word = case (notThe word) of
            Nothing -> "a"
            Just w -> w


isVowel :: Char -> Bool
isVowel ch = ch `elem` "aeiou"

countThe :: [String] -> Integer
countThe [] = 0
countThe (_:[]) = 0
countThe (a:b:xs) = case (notThe a) of
      Nothing -> 
          if( isVowel $ head b) 
          then 1+countThe(b:xs)
          else countThe(b:xs)
      Just _ -> countThe (b:xs)
  


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel  = countThe . words 

countVowels :: String -> Integer
countVowels  = foldr 
                  (\a b -> 
                      if (isVowel a) then b +1
                      else b) 0


newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w  
    | vowels < cons = Just (Word' w)
    | otherwise = Nothing
    where 
      vowels = countVowels w
      cons = (toInteger (length w)) - vowels
      
