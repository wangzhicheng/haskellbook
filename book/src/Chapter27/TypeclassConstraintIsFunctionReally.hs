{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -dsuppress-all #-}

module Chapter27.TypeclassConstraintIsFunctionReally where

a :: Num a => a
a = 1

concrete :: Int
concrete = 1
