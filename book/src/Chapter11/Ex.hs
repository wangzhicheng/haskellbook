module Chapter11.Ex where

import           Data.Char as Char
import           Data.List as List

-------------------------------------------------------
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf aas@(a:as) (b:bs)
  | a == b = isSubseqOf as bs
  | a /= b = isSubseqOf aas bs

-------------------------------------------------------
capitalizedWords :: String -> [(String, String)]
capitalizedWords ss = zip os cs
    where
        os = List.words ss
        cs = fmap capitalizeWord os

-------------------------------------------------------
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord xxs@(x:xs) = if Char.isLower x then Char.toUpper x : xs
                             else xxs

-------------------------------------------------------
endingWithDot :: String -> Bool
endingWithDot [] = False
endingWithDot s  = s !! (length s - 1) == '.'

capitalizeParagraph :: String -> String
capitalizeParagraph para = foldl f "" wl
    where wl = words para
          f acc next
            | endingWithDot acc = acc ++ " " ++ capitalizeWord next
            | otherwise = if acc == ""
                             then capitalizeWord next else acc ++ " " ++ next

