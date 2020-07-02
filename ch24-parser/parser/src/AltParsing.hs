{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah"

b = "123"

c = "123blah789"

eitherOr :: String
eitherOr =
  [r|
123
abc
456
def
  |]

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denomenator <- decimal
  case denomenator of
    0 -> fail "Denomenator can not be zero"
    _ -> return (numerator % denomenator)

type RationalOrDecimal = Either Rational Integer

parseFod :: Parser RationalOrDecimal
parseFod = (Left <$> parseFraction) <|> (Right <$> decimal)

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main :: IO ()
main = do
  let p f i =
        parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  print $ parseString (some parseNos) mempty eitherOr
  print $ parseString parseFod mempty "1/2"
  print $ parseString parseFod mempty "1"
