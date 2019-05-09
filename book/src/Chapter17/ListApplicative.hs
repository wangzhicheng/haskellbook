{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter17.ListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-----------------------------------------------------------------------------
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

-----------------------------------------------------------------------------
instance Semigroup (List a) where
    Nil <> as = as
    as <> Nil = as
    Cons a as <> lr = Cons a (as <> lr)

instance Semigroup (List a) => Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons a as) <*> lr =fmap a lr <> (as <*> lr)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof
        [ return Nil
        , Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Foldable List where
    foldr _ b Nil        = b
    foldr f b (Cons h t) = f h (foldr f b t)

-----------------------------------------------------------------------------
append :: List a -> List a -> List a
append = (<>)

fold :: (a -> b -> b) -> b -> List a -> b
fold = foldr

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' _ Nil        = Nil
take' n (Cons h t) = Cons h (take' (n - 1) t)

-----------------------------------------------------------------------------
main :: IO ()
main = do
    quickBatch $ monoid (undefined :: List Int)
    quickBatch $ functor (undefined :: List (String, String, Int))
    quickBatch $ applicative (undefined :: List (String, String, Int))
