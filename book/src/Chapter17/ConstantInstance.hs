module Chapter17.ConstantInstance where

import           Data.Monoid
import           Test.Hspec

newtype Constant a b = Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant x) <*> (Constant y) = Constant (x <> y)

main :: IO ()
main = hspec $ describe "Constant Applicative <*>" $
            (it "" $ (f <*> g) `shouldBe` Constant (Sum 3))
            >> (it "" $ (pure 1 :: Constant String Int) `shouldBe` Constant "")
                where
                    f = Constant (Sum 1)
                    g = Constant (Sum 2)
