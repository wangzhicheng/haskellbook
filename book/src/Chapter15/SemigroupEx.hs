{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Chapter15.SemigroupEx where

import Test.QuickCheck hiding (Failure, Success)
import Test.Hspec
import Data.Monoid hiding ((<>))

-----------------------------------------------------------------------------
semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-----------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-----------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary (Identity (Product Int)) where
    arbitrary = Identity . Product <$> (arbitrary :: Gen Int)

type IdentityAssoc = Identity (Product Int)
                    -> Identity (Product Int)
                    -> Identity (Product Int)
                    -> Bool

-----------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance Arbitrary (Two (Sum Int) String) where
    arbitrary = do
        si <- arbitrarySumInt
        s  <- arbitraryString
        return $ Two si s
        where 
            arbitrarySumInt = Sum <$> (arbitrary :: Gen Int)
            arbitraryString = arbitrary :: Gen String

type TwoAssoc = 
    Two (Sum Int) String
    -> Two (Sum Int) String
    -> Two (Sum Int) String
    -> Bool

-----------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Three a b c) where
        (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance Arbitrary (Three (Sum Int) All String) where
    arbitrary = do 
        s1 <- asi
        s2 <- aa
        s3 <- as
        return $ Three s1 s2 s3
        where
            asi = Sum <$> (arbitrary :: Gen Int)
            aa  = arbitrary :: Gen All
            as  = arbitrary :: Gen String

type ThreeAssoc =
       Three (Sum Int) All String
    -> Three (Sum Int) All String
    -> Three (Sum Int) All String
    -> Bool

-----------------------------------------------------------------------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

-- The Semigroup for Or should have the following behavior.
-- We can think of this as having a ¡°sticky¡± Snd value where
-- it¡¯ll hold onto the first Snd value when and if one is passed
-- as an argument
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Fst _ <> Snd b   =  Snd b
    Fst _ <> Fst a'  =  Fst a'
    Snd b <> Fst _   =  Snd b
    Snd b <> Snd _'  =  Snd b

instance Arbitrary (Or (Sum Int) String) where
    arbitrary = do 
        s1 <- asi
        s2 <- as
        elements [Fst s1, Snd s2]
        where
            asi = Sum <$> (arbitrary :: Gen Int)
            as  = arbitrary :: Gen String

type OrAssoc =
       Or (Sum Int) String
    -> Or (Sum Int) String
    -> Or (Sum Int) String
    -> Bool

-----------------------------------------------------------------------------
newtype Combine a b = Combine {unCombine :: (a -> b)} 

instance (Semigroup b) => Semigroup (Combine a b) where
    -- Combine (a -> b) <> Combine (a -> b') = Combine (a -> (b <> b'))
    Combine f <> Combine g = Combine (\a -> f a <> g a)

instance Arbitrary (Combine String String) where
    arbitrary = Combine <$> (arbitrary :: Gen (String -> String))


type CombineAssoc =
       Combine String String
    -> Combine String String
    -> Combine String String
    -> Bool

demoCombineF :: SpecWith ()
demoCombineF = describe "associativity of Combine a b" $ do
    it "unCombine (f <> g) $ 0 == Sum 0" $ (unCombine (f <> g) $ 0) `shouldBe` (Sum {getSum = 0})
    it "unCombine (f <> g) $ 1 == Sum 2" $ (unCombine (f <> g) $ 1) `shouldBe` (Sum {getSum = 2})
    it "unCombine (f <> f) $ 1 == Sum 4" $ (unCombine (f <> f) $ 1) `shouldBe` (Sum {getSum = 4})
    it "unCombine (g <> f) $ 1 == Sum 2" $ (unCombine (g <> f) $ 1) `shouldBe` (Sum {getSum = 2})
        where 
            f = Combine $ \n -> Sum (n + 1)
            g = Combine $ \n -> Sum (n - 1)

-----------------------------------------------------------------------------
data Validation a b = 
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Failure a <> Failure a' = Failure (a <> a')
    Failure _ <> Success b = Success b
    Success b <> _ = Success b

demoValidation :: SpecWith ()
demoValidation = describe "Validation" $  do
    it "success 1 <> failure \"blah\"         ==  Success 1"                    
        $  (success 1 <> failure "blah")       `shouldBe`  (Success 1)
    it "failure \"woot\" <> failure \"blah\"  ==  Faulure \"wootblah\""  
        $  (failure "woot" <> failure "blah")  `shouldBe`  (Failure "wootblah")
    it "success 1 <> success 2                ==  Success 1"                           
        $  (success 1 <> success 2)            `shouldBe`  (Success 1)
    it "failure \"woot\" <> success 2         ==  Success 2"                    
        $  (failure "woot" <> success 2)       `shouldBe`  (Success 2)
        where
            failure :: String -> Validation String Int
            failure = Failure
            success :: Int -> Validation String Int
            success = Success


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
--    quickCheck (semigroupAssoc :: CombineAssoc)
    hspec $ demoCombineF
    hspec $ demoValidation
