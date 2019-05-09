{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-type-defaults  #-}

module Chapter15.MonoidEx where

import           Data.Monoid     hiding ((<>))
import           SemigroupEx     hiding (main)
import           Test.Hspec
import           Test.QuickCheck

-----------------------------------------------------------------------------
monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = a <> mempty == a

-----------------------------------------------------------------------------
instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

-----------------------------------------------------------------------------
instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

-----------------------------------------------------------------------------
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

-----------------------------------------------------------------------------
instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine (const mempty)
    mappend = (<>)

demoMonoidCombine :: SpecWith ()
demoMonoidCombine = describe "mempty of Monoid Combine" $ do
    it "unCombine (f <> mempty) 1 == Sum 2" $ (unCombine (f <> mempty) 1) `shouldBe` (Sum 2)
        where
            f = Combine $ \n -> Sum (n + 1)

-----------------------------------------------------------------------------
main :: IO ()
main = do
    quickCheck (monoidLeftIdentity   ::  Trivial -> Bool)
    quickCheck (monoidRightIdentity  ::  Trivial -> Bool)
    quickCheck (monoidLeftIdentity   ::  Identity (Product Int) -> Bool)
    quickCheck (monoidRightIdentity  ::  Identity (Product Int) -> Bool)
    quickCheck (monoidLeftIdentity   ::  Two (Sum Int) String -> Bool)
    quickCheck (monoidRightIdentity  ::  Two (Sum Int) String -> Bool)
    hspec demoMonoidCombine
