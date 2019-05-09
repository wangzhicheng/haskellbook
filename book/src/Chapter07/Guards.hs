module Chapter07.Guards where

bloodNa :: Integer -> String
bloodNa x
  | x < 135   = "too low"
  | x > 145   = "too high"
  | otherwise = "just right"

isRight :: ( Num a, Eq a ) => a -> a -> a -> String
isRight x y z
  | x^2 + y^2 == z^2  =  "Right on"
  | otherwise         =  "not right"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9   =  'A'
  | y >= 0.8   =  'B'
  | y >= 0.7   =  'C'
  | y >= 0.59  =  'D'
  | y < 0.59   =  'F'
  where y = x / 100
