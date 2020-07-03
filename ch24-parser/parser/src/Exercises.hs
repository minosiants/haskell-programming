module Exercises where

import Control.Applicative
import Data.Char (digitToInt, isNumber)
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = do
  ch <- anyChar
  if isNumber ch
    then return ch
    else fail "Not digit"

base10Integer :: Parser Integer
base10Integer = do
  num <- manyTill anyChar letter
  case num of
    [] -> fail "Not digit"
    ('-' : xs) -> return $ -1 * (convert xs)
    _ -> return $ convert num
  where
    convert xs =
      foldl (\ac x -> ac * 10 + toInteger ((digitToInt x))) 0 xs

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
  Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined


main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer mempty "-123abc"
