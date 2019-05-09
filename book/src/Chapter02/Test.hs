{-# LANGUAGE RankNTypes #-}

module Chapter02.Test where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

circleArea :: forall a. Floating a => a -> a
circleArea r = pi * (r * r)
