module Chapter18.EitherInstance where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    First a  <*> _         = First a
    Second _ <*> First a   = First a
    Second b <*> Second b' = Second (b b')

instance Monad (Sum a) where
    return = pure
    First a  >>= _ = First a
    Second b >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = oneof [ First <$> arbitrary
                      , Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: Sum String (String, Int, Int)
        trigger = undefined
    quickBatch $ functor undefined
    quickBatch $ applicative undefined
    quickBatch $ monad undefined
