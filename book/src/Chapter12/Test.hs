module Chapter12.Test where

import           Chapter12.Person
import           Test.Hspec

test_mkPerson :: IO ()
test_mkPerson = hspec $ describe "-------------------" $
    it "" $  show (mkPerson1 "" (-1)) `shouldBe` "Left [NameEmpty,AgeTooLow]"

main :: IO ()
main = do
    test_mkPerson
