module Chapter04.Ex where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs =  xs == reverse xs

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f p1 p2 = ((snd p1, snd p2), (fst p1, fst p2))
-- f (a, b) (c, d) = ((b, d), (a, c))
