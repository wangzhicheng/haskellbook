module Chapter09.Ex where

trimLeadingSpace :: String -> String
trimLeadingSpace = dropWhile (==' ')

takeFirstWord :: String -> String
takeFirstWord = takeWhile (/= ' ') . trimLeadingSpace

dropFirstWord :: String -> String
dropFirstWord = dropWhile (/= ' ') . trimLeadingSpace

myWords :: String -> [String]
myWords "" = []
myWords sentence = takeFirstWord sentence : (myWords . trimLeadingSpace . dropFirstWord $ sentence)
