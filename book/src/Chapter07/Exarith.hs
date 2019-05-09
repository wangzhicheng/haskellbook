module Chapter07.ExArith where

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main :: IO ()
main = do
    print (roundTrip 4)
    print (id 4)
