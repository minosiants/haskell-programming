{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer
      Major
      Minor
      Patch
      Release
      Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  (SemVer ma mi p _ _) `compare` (SemVer ma1 mi1 p1 _ _)
    | ma < ma1 = LT
    | ma == ma1 && mi < mi1 = LT
    | ma == ma1 && mi == mi1 && p < p1 = LT
    | ma == ma1 && mi == mi1 && p == p1 = EQ
    | otherwise = GT

skipSymbol s = skipSome (symbol s)

nosi :: Parser NumberOrString
nosi = NOSI <$> read <$> do
  d <- many digit
  case d of
    [] -> fail "Should not be empty"
    _ -> return d

noss = NOSS <$> many (letter <|> digit)

parseNos :: Parser [NumberOrString]
parseNos = sepBy1 (nosi <|> noss) dot

parsePart :: String -> Parser [NumberOrString]
parsePart s =
  option [] $
    try (skipSymbol s) >> parseNos <|> pure []

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  skipSymbol "."
  minor <- integer
  skipSymbol "."
  patch <- integer
  release <- parsePart "-"
  meta <- parsePart "+"
  return $ SemVer major minor patch release meta

ps = parseString

psv = ps parseSemVer mempty

big = SemVer 2 1 1 [] []

little = SemVer 2 1 0 [] []

main :: IO ()
main = do
  print $ psv "2.1.1"
  print $ psv "1.0.0-x.7.z.92"
  print $ psv "1.0.0-gamma+002"
  print $ psv "1.0.0-beta+oof.sha.41af286"
  print $ big > little
