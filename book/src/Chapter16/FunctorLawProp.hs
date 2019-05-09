{-# LANGUAGE TypeApplications #-}

module Chapter16.FunctorLawProp
    ( functorIdentity
    , functorCompose
    , functorCompose')
    where

import           Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity xx = fmap id xx == xx

functorCompose :: (Functor f, Eq (f c))
               => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g as = (fmap g . fmap f) as == fmap (g . f) as

functorCompose' :: (Functor f, Eq (f c))
                => f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' as (Fun _ f) (Fun _ g) =
    fmap (g . f) as == (fmap g . fmap f) as

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
    quickCheck (functorIdentity :: [Int] -> Bool)
    quickCheck (functorIdentity :: Maybe Int -> Bool)
    quickCheck (functorIdentity :: Either String Int -> Bool)
    verboseCheck (functorCompose' :: IntFC)
