module Chapter10.ScanEx where

import           Data.List

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN = (fibs !! )

firstN :: Int -> [Integer]
firstN n = take n fibs

firstWhile :: (Integer -> Bool) -> [Integer]
firstWhile p = takeWhile p fibs

scanDemo :: IO ()
scanDemo = do
    print $ firstN 20
    print $ firstWhile (<= 100)

factorial :: [Integer]
factorial = scanl (*) 1 [1..]

main :: IO ()
main = scanDemo
