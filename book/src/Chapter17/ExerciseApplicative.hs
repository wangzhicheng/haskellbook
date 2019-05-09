module Chapter17.ExerciseApplicative where

import           Control.Applicative      (liftA2, liftA3)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-----------------------------------------------------------------------------
data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance Semigroup a => Semigroup (Pair a) where
    Pair a1 a2 <> Pair a1' a2' = Pair (a1 <> a1') (a2 <> a2')

instance Monoid a => Monoid (Pair a) where
    mempty = Pair mempty mempty

instance Applicative Pair where
    pure a = Pair a a
    Pair a a' <*> Pair b b' = Pair (a b) (a' b')

-----------------------------------------------------------------------------
data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

instance Monoid a => Applicative (Two a) where
    pure  = Two mempty
    Two a b <*> Two a' b' = Two (a <> a') (b b')

-----------------------------------------------------------------------------
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
      arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

-----------------------------------------------------------------------------
data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Three' a b) where
      arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance (Monoid a) => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' a f g <*> Three' a' b1 b2 = Three' (a <> a') (f b1) (g b2)

-----------------------------------------------------------------------------
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
      arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

-----------------------------------------------------------------------------
data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Four' a b) where
      arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    Four' a1 a2 a3 f <*> Four' a1' a2' a3' b = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

-----------------------------------------------------------------------------
main :: IO ()
main = do
    quickBatch $ functor (undefined :: Pair (String, String, Int))
    quickBatch $ functor (undefined :: Two String (String, String, Int))
    quickBatch $ functor (undefined :: Three Int String (String, String, Int))
    quickBatch $ functor (undefined :: Three' String (String, String, Int))
    quickBatch $ functor (undefined :: Four Int String Int (String, String, Int))
    quickBatch $ functor (undefined :: Four' String (String, String, Int))

    quickBatch $ monoid (undefined :: Pair String)
    quickBatch $ applicative (undefined :: Two (Sum Int) (String, String, Int))
    quickBatch $ applicative (undefined :: Three (Sum Int) (Product Int) (String, String, Int))
    quickBatch $ applicative (undefined :: Three' (Sum Int) (String, String, Int))
    quickBatch $ applicative (undefined :: Four (Sum Int) (Product Int) String (String, String, Int))
    quickBatch $ applicative (undefined :: Four' (Sum Int) (String, String, Int))

