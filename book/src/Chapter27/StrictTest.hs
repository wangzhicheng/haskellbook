{-# LANGUAGE Strict #-}

module Chapter27.StrictTest where

blah x = 1

main = print (blah undefined)
