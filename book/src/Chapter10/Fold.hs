module Chapter10.Fold where

f :: String -> String -> String
f x y = "(" ++ x ++ "+" ++ y ++ ")"

exfr :: String
-- exfr = foldr f "0" $ map show [1..5]
exfr = foldr (f . show) "0" [1..5]

exfl :: String
-- exfl = foldl (f . show) "0" [1..5]
exfl = foldl f "0" $ map show [1..5]

f1 :: String -> Int -> String
f1 str n = "(" ++ str ++ "+" ++ show n ++ ")"

exfl1 :: String
exfl1 = foldl f1 "0" [1..5]

lrex :: IO ()
lrex = do
    putStrLn "--------------------------------------------"
    putStrLn "foldr (^) 2 [1..3]"
    print $ foldr (^) 2 [1..3]

    putStrLn "foldl (^) 2 [1..3]"
    print $ foldl (^) 2 [1..3]

    putStrLn "foldl (flip (^)) 2 [1..3]"
    print $ foldl (flip (^)) 2 [1..3]

    putStrLn "foldl (flip (^)) 2 $ reverse [1..3]"
    print $ foldl (flip (^)) 2 $ reverse [1..3]

lrex1 :: IO ()
lrex1 = do
    putStrLn "--------------------------------------------"
    putStrLn "foldr const '*' ['a'..'k']"
    print $ foldr const '*' ['a'..'k']

    putStrLn "foldr const '*' ['a', undefined, 'b', 'c']"
    print $ foldr const '*' ['a', undefined, 'b', 'c']

    putStrLn "foldr (flip const) '*' ['a'..'k']"
    print $ foldr (flip const) '*' ['a'..'k']

    putStrLn "foldl const '*' ['a', undefined, 'b', 'c']"
    print $ foldl const '*' ['a', undefined, 'b', 'c']

    putStrLn "foldl (flip const ) '*' ['a', undefined, 'b', 'c']"
    print $ foldl (flip const ) '*' ['a', undefined, 'b', 'c']

main :: IO ()
main = do
    lrex
    lrex1
