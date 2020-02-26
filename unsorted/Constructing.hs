module Constructing where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

bess = First (CowInfo "Bess" 4) :: Animal'
elmer' = Second (SheepInfo "Elmer" 5 5)
elmer = Second elmer' :: Animal'

elmo' = Second (SheepInfo "Elmo" 5 5)
-- let elmo = First elmo' :: Animal'