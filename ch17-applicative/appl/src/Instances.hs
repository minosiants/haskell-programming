--Instances.hs

module Instances where

newtype Identity' a = 
  Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)  

instance Applicative Identity' where
  pure a = Identity' a
  (<*>) (Identity' f) (Identity' a) = 
       Identity' (f a) 

newtype Constant a b = 
  Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap f (Constant a) = Constant a 

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a ) (Constant b) = Constant (mappend a b)

  
validateLength ::   Int 
                ->  String
                ->  Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Show, Eq)
newtype Address = Address String deriving (Show, Eq)

mkName :: String -> Maybe Name
mkName s =
  fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  fmap Address $ validateLength 100 a

data Person = 
  Person Name Address deriving (Show, Eq)

mkPerson:: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of 
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of 
        Nothing -> Nothing
        Just a' -> Just (Person n' a')


mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

