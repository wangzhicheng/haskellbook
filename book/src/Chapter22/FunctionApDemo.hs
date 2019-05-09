module Chapter22.FunctionApDemo where

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String 
    deriving (Eq, Show)

data Person = person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogName :: DogName
               , dogsAddress :: Address
               } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
