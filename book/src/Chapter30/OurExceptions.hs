module OurExceptions where

import           Control.Exception

newtype NotDivThree = NotDivThree Int
    deriving (Eq, Show)

instance Exception NotDivThree

newtype NotEven = NotEven Int
    deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

catchNotDivThree :: IO Int
                 -> (NotDivThree -> IO Int)
                 -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int
                -> (NotEven -> IO Int)
                -> IO Int
catchNotEven = catch
