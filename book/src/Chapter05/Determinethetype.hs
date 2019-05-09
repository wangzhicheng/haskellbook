{-# LANGUAGE NoMonomorphismRestriction #-}
module Chapter05.DetermineTheType where

example = 1

i :: a -> a
i = id

c :: a -> b -> a
c = const

c'' :: b -> a -> b
c'' = const

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b = b2c . a2b

a :: (a -> c) -> a -> a
a _ = id

a' :: (a -> b) -> a -> b
a' f x = f x
