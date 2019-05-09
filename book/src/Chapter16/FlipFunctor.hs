{-# LANGUAGE FlexibleInstances #-}

module Chapter16.FlipFunctor where

data Tuple a b = Tuple a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple b a)) = Flip $ Tuple (f b) a

main :: IO ()
main = print $ fmap (+1) (Flip (Tuple 1 "blah"))
