{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Log where

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tuple
import Text.RawString.QQ
import Text.Trifecta

type ActivityType = String

newtype Activity = Activity ActivityType deriving (Show, Ord, Eq)

data Record = Record TimeOfDay Activity deriving (Show, Ord, Eq)

parseTime' :: Parser TimeOfDay
parseTime' = do
  hours <- read <$> count 2 digit
  skipSome (symbol ":")
  minutes <- read <$> count 2 digit
  return $ TimeOfDay hours minutes 0

eol :: Parser ()
eol = symbol "\n" *> return ()

commentSymbol :: Parser ()
commentSymbol =
  do
    (symbol " --")
    manyTill anyChar eol
    return ()

parseActivity :: Parser Activity
parseActivity =
  Activity
    <$> manyTill
      anyChar
      (commentSymbol <|> eol <|> eof)

parseComment :: Parser String
parseComment = do
  hash <- symbol "#"
  rest <- manyTill anyChar (try eol <|> eof)
  return $ hash ++ rest

ss :: Parser ()
ss = symbol " " *> return ()

parseRecord :: Parser Record
parseRecord = do
  skipMany parseComment
  time <- parseTime'
  ss
  activity <- parseActivity
  return $ Record time activity

parseRecords :: Parser [Record]
parseRecords = many (skipMany eol *> parseRecord <* (skipMany eol <|> eof))

type Duration = DiffTime

addDuration :: [Record] -> [(Record, Duration)]
addDuration records = zipWith duration records (drop 1 records)
  where
    duration r@(Record t1 _) (Record t2 _) =
      let (sm1, sm2) = (timeOfDayToTime t1, timeOfDayToTime t2)
       in (r, dur sm1 sm2)
    dur sm1 sm2
      | sm1 <= sm2 = sm2 - sm1
      | otherwise = ((timeOfDayToTime $ TimeOfDay 24 0 0) - sm1) + sm2

sumActivity :: [(Record, Duration)] -> [(Activity, Duration)]
sumActivity r = doSum $ M.fromListWith (++) [(act, [v]) | ((Record _ act), v) <- r]
  where
    doSum =
      M.foldrWithKey
        ( \a d acc ->
            (a, sum d) : acc
        )
        []

logText =
  [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

records = parseString parseRecords mempty logText

main :: IO ()
main = do
  print $ records
  print $ (sumActivity . addDuration) <$> records
