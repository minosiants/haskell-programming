-- GimmePerson.hs

module GimmePerson where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not ( age > 0) = Left AgeTooLow
    | otherwise =
          Left $ PersonInvalidUnknown $
            "Name was: "++ show name ++
            " Age was : "++ show age


gimmePerson :: IO ()
gimmePerson = do
      putStrLn "Enter name"
      name <- getLine
      putStrLn "Enter age"
      age <- getLine
      case (mkPerson name (read age)) of
        Right person -> 
          putStrLn $ "Yay ! successfully got a person: " ++ show person
        Left error -> 
          putStrLn $ "There is an error in person creation: " ++ show error 
