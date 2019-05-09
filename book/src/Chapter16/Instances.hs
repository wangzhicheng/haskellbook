module Chapter16.Instances (main) where

import           Chapter16.FunctorLawProp
import           Test.QuickCheck

-----------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

type IdentityFC = Identity Int -> Fun Int Int -> Fun Int Int -> Bool

identityCheck :: IO ()
identityCheck = do
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose' :: IdentityFC)

-----------------------------------------------------------------------------
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        return $ Pair a1 a2

type PairFC = Pair Int -> Fun Int Int -> Fun Int Int -> Bool

pairCheck :: IO ()
pairCheck = do
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose' :: PairFC)

-----------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeFC = Three Bool String Int -> Fun Int Int -> Fun Int Int -> Bool

threeCheck :: IO ()
threeCheck = do
    quickCheck (functorIdentity :: Three Bool String Int -> Bool)
    quickCheck (functorCompose' :: ThreeFC)

-----------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        return $ Three' a b1 b2

type Three'Fc = Three' String Int -> Fun Int Int -> Fun Int Int -> Bool

three'Check :: IO ()
three'Check = do
    quickCheck (functorIdentity :: Three' String Int -> Bool)
    quickCheck (functorCompose' :: Three'Fc)

-----------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d

type FourFC = Four Bool String Int Int -> Fun Int Int -> Fun Int Int -> Bool

fourCheck :: IO ()
fourCheck = do
    quickCheck (functorIdentity :: Four Bool String Int Int -> Bool)
    quickCheck (functorCompose' :: FourFC)

-----------------------------------------------------------------------------
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        a3 <- arbitrary
        b  <- arbitrary
        return $ Four' a1 a2 a3 b

type Four'FC = Four' String Int -> Fun Int Int -> Fun Int Int -> Bool

four'Check :: IO ()
four'Check = do
    quickCheck (functorIdentity :: Four' String Int -> Bool)
    quickCheck (functorCompose' :: Four'FC)

-----------------------------------------------------------------------------
main :: IO ()
main = do
    identityCheck
    pairCheck
    threeCheck
    three'Check
    fourCheck
    four'Check
