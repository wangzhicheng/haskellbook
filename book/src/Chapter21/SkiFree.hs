module Chapter21.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a 
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> m <*> f a
        where 
            m = traverse f na

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

-- instance ( Applicative n
--          , Testable (n Property)
--          , EqProp a )
--          => EqProp (S n a) where
--     (S x y) =-= (S x' y') =
--         (property $ (=-=) <$> x <*> x')
--         .&. ( y =-= y')
instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

main :: IO ()
main = do
    print =<< sample' (arbitrary :: Gen (S [] Int))
    quickBatch $ traversable (undefined :: S [] (Int, Int, String))
