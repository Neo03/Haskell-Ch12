module Person1 where

--data Either ab = Left a | Right b

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty
  |AgeToLow
  deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
   True -> Right age
   False -> Left [AgeToLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name)(ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge )
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge)  = Left badAge