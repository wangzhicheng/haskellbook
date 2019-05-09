module Chapter17.ZipListMonoid where

import           Data.Monoid              (Sum)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype ZList a = ZList [a]
    deriving (Eq, Show)

instance Semigroup a => Semigroup (ZList a) where
    ZList as <> ZList as' = ZList $ zipWith (<>) as as'

instance Monoid a => Monoid (ZList a) where
    mempty = ZList $ repeat mempty

instance Arbitrary a => Arbitrary (ZList a) where
    arbitrary = ZList <$> arbitrary

instance Eq a => EqProp (ZList a) where
    (=-=) = eq

main :: IO ()
main = quickBatch $ monoid $ ZList [1 :: Sum Int]
