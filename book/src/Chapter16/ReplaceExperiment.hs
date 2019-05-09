module Chapter16.ReplaceExperiment where

import           Test.Hspec

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f, Functor g)
            => f (g a) -> f (g Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f, Functor g, Functor h)
             => f (g (h a)) -> f (g (h Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

check :: SpecWith ()
check = describe " composed fmap over stacking functor" $ do
    it "" $ (replaceWithP' lms) `shouldBe` 'p'
    it "" $ (liftedReplace lms) `shouldBe` "ppp"
    it "" $ (liftedReplace' lms) `shouldBe` "ppp"
    it "" $ (twiceLifted lms) `shouldBe` [Just 'p', Nothing, Just 'p']
    it "" $ (twiceLifted' lms) `shouldBe` [Just 'p', Nothing, Just 'p']
    it "" $ (thriceLifted lms) `shouldBe` [Just "ppp", Nothing, Just "pppppp"]


main :: IO ()
main = hspec $ check
