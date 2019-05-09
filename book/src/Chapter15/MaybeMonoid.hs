{-# LANGUAGE FlexibleInstances #-}

module Chapter15.MaybeMonoid where

import           Optional
import           Test.QuickCheck

------------------------------------------------------------------
newtype First' a = First' {getFirst' :: Optional a}
    deriving (Eq, Show)

instance Arbitrary (First' String) where
    arbitrary =
        frequency [ (1, return (First' Nada))
                  , (2, return (First' (Only "wzc")))
                  , (2, return (First' (Only "good")))
                  , (2, return (First' (Only "awesome")))
                  , (2, return (First' (Only "chinese")))
                  , (4, return (First' (Only "donald trump")))]

instance Semigroup (First' a) where
    First' Nada         <>  First' Nada         =  First' Nada
    First' Nada         <>  (First'  (Only a))  =  First'  (Only a)
    (First'  (Only a))  <>  First' Nada         =  First'  (Only a)
    (First'  (Only a))  <>  (First'  (Only _))  =  First'  (Only a)

instance Monoid (First' a) where
    mempty  =  First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = (<>)

------------------------------------------------------------------
type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId = First' String -> Bool

------------------------------------------------------------------
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc = \x y z -> ((x <> y) <> z) == (x <> (y <> z))

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity = \x -> mempty <> x == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity = \x -> x <> mempty == x

------------------------------------------------------------------
main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)

