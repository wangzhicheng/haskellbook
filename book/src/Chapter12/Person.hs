module Chapter12.Person where

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0  =  Right $ Person name age
  | name == ""             =  Left NameEmpty
  | otherwise              =  Left AgeTooLow

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
                 True  -> Right age
                 False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
                  True  -> Right name
                  False -> Left [NameEmpty]

mkPerson1 :: Name -> Age -> ValidatePerson Person
mkPerson1 name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right $ Person nameOk ageOk
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _              (Left badAge) = Left badAge

