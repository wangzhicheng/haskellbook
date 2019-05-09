module Chapter17.ZipListApplicative where

import           Chapter17.ListApplicative hiding (main)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-----------------------------------------------------------------------------
newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

-----------------------------------------------------------------------------
instance Functor ZipList' where
    fmap f (ZipList' l) = ZipList' $ fmap f l

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                   in take' 3000 l
            ys' = let (ZipList' l) = ys
                   in take' 3000 l

-----------------------------------------------------------------------------
instance Semigroup a => Semigroup (ZipList' a) where
    ZipList' ll <> ZipList' lr = ZipList' $ zipWith' (<>) ll lr
        where
            zipWith' _ Nil _ = Nil
            zipWith' _ _ Nil = Nil
            zipWith' f (Cons a as) (Cons a' as') = Cons (f a a') (zipWith' f as as')

instance Monoid a => Monoid (ZipList' a) where
    mempty = ZipList' $ repeat' mempty
        where
            repeat' x = Cons x (repeat' x)

instance Applicative ZipList' where
    pure a = ZipList' $ go a
        where
            go x = Cons x (go x)
    ZipList' lf <*> ZipList' la = ZipList' $ go lf la
        where
            go (Cons f fs) (Cons a as) = Cons (f a) (go fs as)
            go _ _                     = Nil

-----------------------------------------------------------------------------
main :: IO ()
main = do
    quickBatch $ functor (undefined :: ZipList' (String, String, Int))
    quickBatch $ monoid (undefined :: ZipList' (Sum Int))
    quickBatch $ applicative (undefined :: ZipList' (String, String, Int))
