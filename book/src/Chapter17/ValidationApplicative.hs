module Chapter17.ValidationApplicative where

import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a = Failure e | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure =  Success
    Failure e <*> Failure e'  =  Failure (e <> e')
    Failure e <*> _           =  Failure e
    Success _ <*> Failure e   =  Failure e
    Success a <*> Success a'  =  Success (a a')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

main :: IO ()
main = quickBatch $ applicative (undefined :: (Validation String) (String, String, Int))

