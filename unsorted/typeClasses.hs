-- typeClasses.hs

module TypeClasses where 

data Trivial = Trivial'

instance Eq Trivial where 
  Trivial' == Trivial' = True


data DayOfWeek  = 
  Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

data Date  = 
  Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where 
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False 

instance  Eq Date where
  (==) (Date weekday dayOfMonth) 
       (Date weekday' dayOfMonth') =
        weekday == weekday' 
     && dayOfMonth == dayOfMonth'


data Identety a = 
  Identety a deriving Show

instance Eq a => Eq (Identety a) where 
 (==) (Identety v) (Identety v') = v == v'






data TisAnInteger = 
  TisAn Integer 

instance Eq TisAnInteger where 
  (==) (TisAn i) (TisAn i') = i==i' 

data TwoIntegers = 
 Two Integer Integer

instance Eq TwoIntegers where 
  (==) (Two i ii) (Two i' ii') = i==i' && ii==ii'

data StringOrInt = 
   TisAnInt Int
 | TisAnString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i==i'
  (==) (TisAnString s) (TisAnString s') = s==s'
  (==) _ _ = False


data Pair a =
  Pair a a
instance Eq a => Eq (Pair a) where 
  (==) (Pair a aa) (Pair a' aa') = a == a' && aa == aa'

data Tuple a b = 
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
   ThisOne a 
 | ThatOne a

instance Eq a => Eq (Which a) where 
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
   Hello a
 | Goodbay b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbay b) (Goodbay b') = b ==b'
  (==) _ _ = False
 
