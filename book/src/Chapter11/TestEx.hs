module Chapter11.TestEx where

import           Chapter11.Ex
import           Chapter11.HuttonsRazor
import           Test.Hspec

test_isSubseqOf :: IO ()
test_isSubseqOf = hspec $
    describe "--- isSubseqOf ---" $ do
        it "" $ isSubseqOf "blah" "blahwoot" `shouldBe` True
        it "" $ isSubseqOf "blah" "wootblah" `shouldBe` True
        it "" $ isSubseqOf "blah" "wbolath"  `shouldBe` True
        it "" $ isSubseqOf "blah" "wohtbla"  `shouldBe` False
        it "" $ isSubseqOf "blah" "halbwoot" `shouldBe` False

test_capitalizedWords :: IO ()
test_capitalizedWords = hspec $
    describe "----captializedWords ----" $
        it "" $ capitalizedWords "hello world" `shouldBe`
            [("hello", "Hello"), ("world", "World")]

test_capitalizeParagraph :: IO ()
test_capitalizeParagraph = hspec $
    describe "----- capitalizeParagraph -----" $
        it "" $  capitalizeParagraph "blah. woot ha."
                `shouldBe` "Blah. Woot ha."

test_printExpr :: IO ()
test_printExpr = hspec $
    describe "----- printExpr -----" $ do
        it "" $  printExpr (Add (Lit 1) (Lit 9001)) `shouldBe` "1 + 9001"
        it "" $  printExpr ex `shouldBe` "1 + 9001 + 1 + 20001"
                    where ex = Add (Lit 1) $ Add (Add (Lit 9001) (Lit 1)) (Lit 20001)


main :: IO ()
main = do
    test_isSubseqOf
    test_capitalizeParagraph
    test_capitalizedWords
    test_printExpr

