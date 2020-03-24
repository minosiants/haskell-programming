{-# LANGUAGE FlexibleInstances
               #-}
 -- adt..hs

module Adt where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = 
           Mini
         | Mazda
         | Tata
         deriving (Eq, Show)

data Airline = 
           PapuAir
         | CatapultsR'Us
         | TakeYourChancesUnited
         deriving (Eq, Show)

data Size = Size Int deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
              | Plane Airline Size
              deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 500)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = foldr 
              (\a b -> 
                   isCar a && b) True
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tomanyGoats :: Goats -> Bool
tomanyGoats (Goats n) = n > 42

class TooMany a where 
   tooMany :: a -> Bool

instance TooMany Int where
   tooMany n = n > 42

instance TooMany Goats where
   tooMany (Goats n) = n > 42 

newtype Goats' = Goats' (Int, String) deriving (Eq, Show)

instance TooMany Goats' where 
  tooMany (Goats'(i, s)) = tooMany i && length s > 42  

instance TooMany (Int, Int) where
  tooMany (i, i') = tooMany (i+i')

instance (Num a , TooMany a) => TooMany (a, a) where
  tooMany (i, i') = tooMany (i+i')

data Person = 
   Person { name :: String
          , age :: Int } 
          deriving (Show, Eq)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
               | NonfictionBook Nonfiction
               deriving Show

type AuthorName = String





data GuessWhat = 
   Chickenbutt deriving (Eq, Show)

data Id a = 
   MkId a deriving (Eq, Show)

data Product a b =
   Product a b deriving (Eq, Show)

data Sum a b =
    First a 
  | Second b 
  deriving (Eq, Show)

data RecordProduct a b =
   RecordProduct { pfirst :: a
                 , psecond :: b }
                 deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig 

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMug = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMug deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = 
     CowInfo
   | PigInfo
   | SheepInfo
   deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

