{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Chapter15.RunMemMonoid where

import           Test.Hspec

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
    msa1 <> msa2 = Mem f
        where
            f s = (a1 <> a2, s2)
                where
                    (a1, s1) = runMem msa1 s
                    (a2, s2) = runMem msa2 s1

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (mempty,)

f' :: Num s => Mem s String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = hspec $ do
    describe "newtype Mem s a monoid instance" $ do
      it "rmleft == (\"hi\",1)"   $ rmleft `shouldBe` ("hi",1)
      it "rmright == (\"hi\",1)"   $ rmright `shouldBe` ("hi",1)
      it "(rmzero :: (String, Int) == (\"\",0)"   $ (rmzero :: (String, Int)) `shouldBe` ("",0)
      it "rmleft == runMem f' 0"   $ rmleft == runMem f' 0
      it "rmright == runMem f' 0"   $ rmright == runMem f' 0
          where
                rmzero = runMem mempty 0
                rmleft = runMem (f' <> mempty) 0
                rmright = runMem (mempty <> f') 0
