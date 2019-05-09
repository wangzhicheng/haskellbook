{-# LANGUAGE TypeApplications #-}

module Chapter26.Exercises where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT (\a -> Identity (a - 1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "hello, " ++ show a
    return (a + 1)

sPrintInAccum :: (Num a, Show a)
              => StateT a IO String
sPrintInAccum = StateT (\a -> do
    putStrLn ("hi " ++ show a)
    return (show a, a + 1))
