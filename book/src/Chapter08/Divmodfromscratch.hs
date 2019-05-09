module Chapter08.DivModFromScratch where

data DividedResult = Result (Integer, Integer) | DividedByZero
    deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = Result $ go num denom 0
    where go n d count
            | n >= 0  &&  d >= 0  &&  n < d  = (count, n)
            | n >= 0  &&  d >= 0  &&  n >=d  = go (n - d) d (count + 1)
            | n <  0  &&  d >= 0  = f $ go (-n) d 0
            | n <  0  &&  d <  0  = g $ go (-n) (-d) 0
                where f (cc, nn)
                        | nn == 0   = (cc, -nn)
                        | otherwise = (-cc - 1, d - nn)
                      g (cc, nn)
                        | nn == 0   = (cc, -nn)
                        | otherwise = (cc + 1, -d - nn)
