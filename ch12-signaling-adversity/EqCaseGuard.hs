-- EqCaseGuard.hs

module EqCaseGuard where


type Name = String
type Age = Integer 

type ValidatePerson a = Either[PersonInvalid] a 

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeToLow
                   deriving (Eq, Show)



ageOkey :: Age -> ValidatePerson Age
ageOkey age = case age >= 0 of 
      True -> Right age
      False -> Left[AgeToLow]

nameOkey :: Name -> ValidatePerson Name
nameOkey name = case name /= "" of
     True -> Right name
     False -> Left[NameEmpty]

     
      

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkey name) (ageOkey age)


mkPerson' :: ValidatePerson Name 
         -> ValidatePerson Age
         -> ValidatePerson Person

mkPerson' (Right name) (Right age) = Right $ Person name age
mkPerson' (Left name) (Left age) = Left(name ++ age)
mkPerson' (Left name) _ = Left(name)
mkPerson' _ (Left age) = Left(age)


