{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter11.Newtypevstype where

newtype Goats = Goats Int deriving (Eq, Show)

newtype Cows = Cows Int deriving (Eq, Show, TooMany)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 44

-- {-# LANGUAGE GeneralizedNewTypeDeriving #-}
-- instance TooMany Cows where
-- tooMany (Cows n) = tooMany n

demo :: IO ()
demo = do
    print $ tooMany (43 :: Int)
    print $ tooMany (Goats 43)

main :: IO ()
main = demo

