module Chapter09.MyStdLib where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myAnd1 :: [Bool] -> Bool
myAnd1 []     = True
myAnd1 (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = myOr . map p

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = x == y || myElem x ys

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = myOr .  map (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy =  undefined

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
