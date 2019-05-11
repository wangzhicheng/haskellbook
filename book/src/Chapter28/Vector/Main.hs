{-# LANGUAGE TupleSections #-}

module Main where

import           Criterion.Main
import           Data.Vector    ((//))
import qualified Data.Vector    as V

slice :: Int -> Int -> [a] -> [a]
slice from len xs =
    take len (drop from xs)

l :: [Int]
l = [1..10000]

vec :: V.Vector Int
vec = V.fromList l

slow :: Int -> V.Vector Int
slow n = go n vec
    where go 0 v = v
          go n v = go (n - 1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
    where updates = (,0) <$> [0..n]

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
    where updates = (,0) <$> V.fromList [0..n]

main :: IO ()
main = defaultMain
    [ bench "slicing list" $ whnf (head . slice 100 900)  l
    , bench "slicing vector" $ whnf (V.head . V.slice 100 900) vec
    , bench "slow" $ whnf slow 9998
    , bench "batch list" $ whnf batchList 9998
    , bench "batch vector" $ whnf batchVector 9998
    ]
