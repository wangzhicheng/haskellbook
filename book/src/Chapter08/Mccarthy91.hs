module Chapter08.McCarthy91 where

mc91 :: Integer -> Integer
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ n + 11

main :: IO ()
main = do
    print $ map mc91 [95..110]
