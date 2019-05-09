module Chapter09.Cipher where

import           Data.Char

shiftChar :: Int  -> Char -> Char
shiftChar n = chr . (+n) . ord

caesar :: Int -> String -> String
caesar n str = map (shiftChar n) str

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
