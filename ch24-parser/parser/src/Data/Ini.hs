{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

newtype Header
  = Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assigmentEx :: ByteString
assigmentEx = "woot=1"

type Name = String

type Value = String

type Assignments = Map Name Value

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (oneOf "\n")
  skipEOL
  return (name, val)

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        _ <- char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL
    )

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' =
  [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' =
  [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section
  = Section Header Assignments
  deriving (Eq, Show)

newtype Config
  = Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section h (M.fromList assignments)


rollup :: Section
    -> Map Header Assignments
    -> Map Header Assignments
rollup (Section h a) m =
    M.insert h a m

parseIni :: Parser Config
parseIni = do
      sections <- some parseSection
      let mapOfSections =
            foldr rollup M.empty sections
      return (Config mapOfSections)
