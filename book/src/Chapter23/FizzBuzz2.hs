module Chapter23.FizzBuzz2 where

import           Control.Monad
import           Control.Monad.Trans.State
import           FizzBuzz1                 (fizzBuzz)

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
