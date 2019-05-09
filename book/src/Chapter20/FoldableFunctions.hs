{-# LANGUAGE ScopedTypeVariables #-}

module Chapter20.FoldableFunctions where

import           Data.Monoid (Any (..))
import           Prelude     hiding (elem, length, maximum, minimum, null,
                              product, sum)

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a s = getAny $ foldMap (Any . (== a)) s

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr minM Nothing
    where minM x Nothing   = Just x
          minM x (Just x') = Just (min x x')

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr maxM Nothing
    where maxM x Nothing   = Just x
          maxM x (Just x') = Just (max x x')

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ l -> 1 + l) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

filterF ::  forall f t a. (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p = foldMap m
    where m :: a -> f a
          m a = if p a then pure a else mempty
