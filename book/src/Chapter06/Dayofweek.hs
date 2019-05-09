module Chapter06.DayOfWeek where

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False

instance Eq Date where
    (==) (Date weekDay dayOfMonth)
         (Date weekDay' dayOfMonth') =
         weekDay == weekDay' && dayOfMonth == dayOfMonth'

------------------------------------------------------------
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn i') = i == i'

------------------------------------------------------------
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i j)
         (Two i' j') =
         i == i' && j == j'


------------------------------------------------------------
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt i')         = i == i'
    (==) (TisAString str) (TisAString str') = str == str'
    (==) _ _                                = False

------------------------------------------------------------
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

------------------------------------------------------------
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

------------------------------------------------------------
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _                      = False

------------------------------------------------------------
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x')     = x == x'
    (==) (Goodbye x) (Goodbye x') = x == x'
    (==) _ _                      = False
