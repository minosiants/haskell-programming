module PhoneNumber where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber
  = PhoneNumber
      NumberingPlanArea
      Exchange
      LineNumber
  deriving (Eq, Show)

daches = nesting . between (symbolic '-') (symbolic '-')

parsePlanAre :: Parser NumberingPlanArea
parsePlanAre = do
  v <- parens (count 3 digit) <|> count 3 digit
  return $ fromIntegral $ read v

parseExchange :: Parser Exchange
parseExchange = do
  v <- daches (count 3 digit) <|> count 3 digit
  return $ fromIntegral $ read v

parseLineNumber :: Parser LineNumber
parseLineNumber = do
  skipMany (symbol "-")
  i <- count 4 digit
  return $ fromIntegral $ read i

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipMany (string "1-")
  pa <- parsePlanAre
  e <- parseExchange
  ln <- parseLineNumber
  return $ PhoneNumber pa e ln

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
