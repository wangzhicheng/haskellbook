module Chapter18.MonadInstances where

import           Prelude                  hiding (Left, Right)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-----------------------------------------------------------------------------
data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

test_nope :: IO ()
test_nope = do
    let trigger :: Nope (Int, Int, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

-----------------------------------------------------------------------------
data PhhhbbtttEither b a = Left a | Right b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = oneof [ Left <$> arbitrary
                      , Right <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

instance Functor (PhhhbbtttEither b) where
    fmap f (Left a)  =  Left (f a)
    fmap _ (Right b) =  Right b

instance Applicative (PhhhbbtttEither b) where
    pure =  Left

    Left a   <*>  Left a'  =  Left (a a')
    Right b  <*>  _        =  Right b
    _        <*>  Right b  =  Right b

instance Monad (PhhhbbtttEither b) where
    return = pure

    Right b >>= _ = Right b
    Left a >>= f  = f a

test_phhhbbtttEither :: IO ()
test_phhhbbtttEither = do
    let trigger :: PhhhbbtttEither String (Int, Int, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

-----------------------------------------------------------------------------
newtype Identity a = Identity a
    deriving (Eq, Show, Ord)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity

    Identity a <*> Identity a' = Identity $ a a'

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

test_identity :: IO ()
test_identity = do
    let trigger :: Identity (Int, Int, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

-----------------------------------------------------------------------------

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [ return Nil
                      , Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Functor List where
    fmap _ Nil         =  Nil
    fmap f (Cons a as) =  Cons (f a) (fmap f as)

instance Semigroup (List a) where
    Nil <> l = l
    Cons a as <> l = Cons a (as <> l)

instance Monoid (List a) where
    mempty = Nil

instance Applicative List where
    pure a = Cons a Nil

    Nil        <*>  _    =  Nil
    Cons _ _   <*>  Nil  =  Nil
    Cons a as  <*>  l    =  fmap a l <> (as <*> l)

instance Monad List where
    return = pure

    Nil >>= _ = Nil
    Cons a as >>= f = f a <> (as >>= f)

test_list :: IO ()
test_list = do
    let trigger :: List (Int, Int, Int)
        trigger = undefined
    quickBatch $ monoid (undefined :: List String)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

-----------------------------------------------------------------------------
main :: IO ()
main = do
    test_nope
    test_phhhbbtttEither
    test_identity
    test_list
