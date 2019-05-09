module Chapter07.Ex where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit1 :: Integral a => a -> a
tensDigit1 = flip mod 10 . fst . flip divMod 10

hundrendsDigit :: Integral a => a -> a
hundrendsDigit = flip mod 10 . fst . flip divMod 100

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
      True  -> x
      False -> y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
  | b          = x
  | otherwise  = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b  = if b then x else y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ True  = x
foldBool3 _ y False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

