module Chapter10.Ex where

--------------------------------------------
svsTuple :: String -> String -> [ (Char, Char, Char)]
svsTuple xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

pTuple :: String -> String -> [ (Char, Char, Char)]
pTuple xs ys = [('p', y, z) | y <- ys, z <- xs]

nvnTuple :: [ String ] -> [ String ] -> [ (String, String, String)]
nvnTuple xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

ex1Demo :: IO ()
ex1Demo = do
    print $ svsTuple stops vowels
    print $ pTuple stops vowels
        where
            stops = "pbtdkg"
            vowels = "aeiou"

--------------------------------------------
-- Average length of words in a sentence
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

ex2Demo :: IO ()
ex2Demo = print $ seekritFunc "Donald Trump Make America Great Again!"

--------------------------------------------
-----ReWriting functions using folds -------
--------------------------------------------

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr ((||) . p) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = foldr ((||) . (== x)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr f []
    where f x y = if p x then x : y else y

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- FIXME
myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "input is an empty list"
myMaximumBy cp (x:xs) = foldr f x (x:xs)
    where f x y = if cp x y == GT then x else y

-- TODO
myMinimuxBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimuxBy = undefined

maximumByDemo :: IO ()
maximumByDemo = do
    print $ myMaximumBy (\_ _ -> GT) [1..10]
    print $ myMaximumBy (\_ _ -> LT) [1..10]
    print $ myMaximumBy compare [1..10]

main :: IO ()
main = do
    print "--------------------------------------------"
    ex1Demo

    print "--------------------------------------------"
    ex2Demo

    print "--------------------------------------------"
    maximumByDemo
