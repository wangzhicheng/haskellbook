module Chapter11.Vehicle where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Int
    deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _ ) = True
isPlane _            = False

areCars :: [Vehicle] -> Bool
areCars = and . fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = error "not a Car"

ex1106 :: IO ()
ex1106 = print myCar >> print urCar >> print clownCar >> print dodge
             where
                myCar    = Car Mini (Price 14000)
                urCar    = Car Mazda (Price 20000)
                clownCar = Car Tata (Price 7000)
                dodge    = Plane PapuAir 888

main :: IO ()
main = ex1106


