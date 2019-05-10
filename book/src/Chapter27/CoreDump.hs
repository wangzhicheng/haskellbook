{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -dsuppress-all #-}

module Chapter27.CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
    let x = undefined
    in case b of
      False -> 0
      True  -> 1

discriminatory2 :: Bool -> Int
discriminatory2 b =
    let x = undefined
    in case x `seq` b of
      False -> 0
      True  -> 1
