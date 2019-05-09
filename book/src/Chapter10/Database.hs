module Chapter10.DatabaseProc where

import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate = foldr ((++) . acceptOnlyDbDate) []
filterDbDate = concatMap acceptOnlyDbDate
    where
        acceptOnlyDbDate :: DatabaseItem -> [UTCTime]
        acceptOnlyDbDate (DbDate t) = [ t ]
        acceptOnlyDbDate _          = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = concatMap acceptOnlyDbNumber
    where
        acceptOnlyDbNumber :: DatabaseItem -> [ Integer ]
        acceptOnlyDbNumber (DbNumber n) = [n]
        acceptOnlyDbNumber _            = []

sumDb :: [DatabaseItem] -> Integer
sumDb  = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb dis = (fromIntegral . sumDb $ dis) / (fromIntegral . length .  filterDbDate $ dis)

test :: IO ()
test = do
    print $ mostRecent theDatabase
    print $ sumDb theDatabase
    print $ avgDb theDatabase

main :: IO ()
main = test
