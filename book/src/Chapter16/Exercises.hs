{-# LANGUAGE FlexibleInstances #-}

module Chapter16.Exercises where

import           Chapter15.FunctorLawProp
import           Test.QuickCheck

-----------------------------------------------------------------------------
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = oneof [ return Finance
                      , Desk <$> arbitrary
                      , Bloor <$> arbitrary]

type QuantFC = Quant String Int -> Fun Int Int -> Fun Int Int -> Bool

quantCheck :: IO ()
quantCheck = do
    quickCheck (functorIdentity :: Quant String Int -> Bool)
    quickCheck (functorCompose' :: QuantFC)

-----------------------------------------------------------------------------
newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

-----------------------------------------------------------------------------
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K b )) = Flip $ K (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
    arbitrary = Flip . K <$> arbitrary

type FlipKFC = Flip K String Int -> Fun Int Int -> Fun Int Int -> Bool

flipKCheck :: IO ()
flipKCheck = do
    quickCheck (functorIdentity :: Flip K String Int -> Bool)
    quickCheck (functorCompose' :: FlipKFC)

-----------------------------------------------------------------------------
newtype EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = GoatyConst <$> arbitrary

type EvilFC = EvilGoateeConst String Int -> Fun Int Int -> Fun Int Int -> Bool

evilCheck :: IO ()
evilCheck = do
    quickCheck (functorIdentity :: EvilGoateeConst String Int -> Bool)
    quickCheck (functorCompose' :: EvilFC)

-----------------------------------------------------------------------------
newtype LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut as) = LiftItOut $ fmap g as

instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
    arbitrary =
        frequency [ (1, return $ LiftItOut Nothing)
                  , (3, LiftItOut . Just <$> arbitrary)]

type LiftItOutFC = LiftItOut Maybe String
                 -> Fun String String
                 -> Fun String String
                 -> Bool

liftItOutCheck :: IO ()
liftItOutCheck = do
    quickCheck (functorIdentity :: LiftItOut Maybe String -> Bool)
    quickCheck (functorCompose' :: LiftItOutFC)

-----------------------------------------------------------------------------
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa as1 as2) = DaWrappa (fmap h as1) (fmap h as2)

instance (Arbitrary a) => Arbitrary (Parappa [] Maybe a) where
    arbitrary = DaWrappa <$> arbitrary <*> am
        where
            am = frequency [(1, return Nothing)
                           , (3, Just <$> arbitrary)]

type ParappaFC = Parappa [] Maybe Int
                -> Fun Int Int
                -> Fun Int Int
                -> Bool

parappaCheck :: IO ()
parappaCheck = do
    quickCheck (functorIdentity :: Parappa [] Maybe Int -> Bool)
    quickCheck (functorCompose' :: ParappaFC)

-----------------------------------------------------------------------------
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething as bs) = IgnoringSomething as (fmap f bs)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (IgnoreOne Maybe [] a b) where
      arbitrary = IgnoringSomething <$> am <*> arbitrary
          where
              am = frequency [ (1, return Nothing)
                             , (3, Just <$> arbitrary)]

type IgnoreOneFC = IgnoreOne Maybe [] String Int
                 -> Fun Int Int
                 -> Fun Int Int
                 -> Bool

ignoreOneCheck :: IO ()
ignoreOneCheck = do
    quickCheck (functorIdentity :: IgnoreOne Maybe [] String Int -> Bool)
    quickCheck (functorCompose' :: IgnoreOneFC)

-----------------------------------------------------------------------------
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-----------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        frequency [ (1, return Nil)
                  , (99, Cons <$> arbitrary <*> arbitrary)]

type ListFC = List Int -> Fun Int Int -> Fun Int Int -> Bool

listCheck :: IO ()
listCheck = do
    quickCheck (functorIdentity :: List Int -> Bool)
    quickCheck (functorCompose' :: ListFC)

-----------------------------------------------------------------------------
data GoatLord a = NoGoat
      | OneGoat a
      | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
      deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat       =  NoGoat
    fmap f (OneGoat a)  =  OneGoat (f a)
    fmap f (MoreGoats as1 as2 as3) =
        MoreGoats (fmap f as1) (fmap f as2) (fmap f as3)

instance Arbitrary a => Arbitrary (GoatLord a) where
    arbitrary = oneof [ return NoGoat
                      , OneGoat <$> arbitrary
                      , MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary]

type GoatLordFC = GoatLord Int
                -> Fun Int Int
                -> Fun Int Int
                -> Bool

goatLordCheck :: IO ()
goatLordCheck = do
    quickCheck (functorIdentity :: GoatLord Int -> Bool)
    quickCheck (functorCompose' :: GoatLordFC)

-----------------------------------------------------------------------------
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g)    = Read (f . g)

instance Arbitrary a => Arbitrary (TalkToMe a) where
    arbitrary = oneof
        [ return Halt
        , Print <$> arbitrary <*> arbitrary
        , Read <$> arbitrary]

-- TODO: quick check
-- talkToMeCheck :: IO ()
-- talkToMeCheck = quickCheck (functorIdentity :: TalkToMe Int -> Bool)

-----------------------------------------------------------------------------
main :: IO ()
main = do
    quantCheck
    flipKCheck
    evilCheck
    liftItOutCheck
    parappaCheck
    ignoreOneCheck
    listCheck
    goatLordCheck
