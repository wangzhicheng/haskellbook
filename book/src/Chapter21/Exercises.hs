module Chapter21.Exercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-----------------------------------------------------------------------------
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

test_identity :: IO ()
test_identity = do
    quickBatch $ functor (undefined :: Identity (String, String, Int))
    quickBatch $ traversable (undefined :: Identity (Int, Int, String))

-----------------------------------------------------------------------------
newtype Constant a b = Constant {  getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

test_constant :: IO ()
test_constant = do
    quickBatch $ functor (undefined :: Constant Int (String, String, Int))
    quickBatch $ traversable (undefined :: Constant Int (Int, Int, String))

-----------------------------------------------------------------------------
data Optional a = Nada | Yep a
    deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [ return Nada
                      , Yep <$> arbitrary
                      ]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

test_optional :: IO ()
test_optional = do
    quickBatch $ functor (undefined :: Optional (String, String, Int))
    quickBatch $ traversable (undefined :: Optional (Int, Int, String))
-----------------------------------------------------------------------------
data List a = Nil | Cons a (List a)
    deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [ return Nil
                      , Cons <$> arbitrary <*> arbitrary
                      ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

test_list :: IO ()
test_list = do
    quickBatch $ functor (undefined :: List (String, String, Int))
    quickBatch $ traversable (undefined :: List (Int, Int, String))

-----------------------------------------------------------------------------
data Three a b c = Three a b c
    deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
      arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

test_three :: IO ()
test_three = do
    quickBatch $ functor (undefined :: Three Int Int (String, String, Int))
    quickBatch $ traversable (undefined :: Three String Int (Int, Int, String))

-----------------------------------------------------------------------------
data Pair a b = Pair a b
    deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

test_pair :: IO ()
test_pair = do
    quickBatch $ functor (undefined :: Pair Int (String, String, Int))
    quickBatch $ traversable (undefined :: Pair Int (Int, Int, String))

-----------------------------------------------------------------------------
data Big a b = Big a b b
    deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
    foldMap f (Big _ b1 b2) = f b1 <> f b2

instance Traversable (Big a) where
    traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

test_big :: IO ()
test_big = do
    quickBatch $ functor (undefined :: Big Int (String, String, Int))
    quickBatch $ traversable (undefined :: Big Int (Int, Int, String))

main :: IO ()
main = do
    test_identity
    test_constant
    test_optional
    test_list
    test_three
    test_pair
    test_big
