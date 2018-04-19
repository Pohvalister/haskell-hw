module TestUnit3 where

import Test.Tasty (TestTree)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Control.Applicative (Alternative(..))
import Unit3 (Parser(..), ok, eof,isnot,satisfy,element,stream,trueBrackets,int)

import Data.Maybe (isJust, isNothing)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "parser_test" specParsers
-- to test HSPEC: runhaskell "this file"

specParsers :: Spec
specParsers = do
  describe "simple combinators test" $ do
    it "ok comb - always Just" $
      runParser ok "anything" `shouldSatisfy` isJust
    it "isnot with ok - always Nothing" $
      runParser (isnot ok) "anything" `shouldSatisfy` isNothing
    it "eof fails in no empty list" $
      runParser eof [3,2] `shouldSatisfy` isNothing
    it "eof with empty str" $
      runParser eof "" `shouldBe` Just ((),"")
    it "satisfy (True) - always Just" $
      runParser (satisfy (const True)) [1,1] `shouldBe` Just (1, [1])
    it "satisfy symbol" $
      runParser (satisfy ('c'==)) "string" `shouldSatisfy` isNothing
  describe "element and stream tests" $ do
    it "test element" $
      runParser (element 1) [1,2,3] `shouldBe` Just (1, [2,3])
    it "test wrong element" $
      runParser (element "q") ["", "wqe"] `shouldBe` Nothing
    it "test empty element" $
      runParser (element 'c') "" `shouldBe` Nothing
    it "test stream" $
      runParser (stream "lot") "lots of values" `shouldBe` Just ("lot", "s of values")
    it "test wrong stream" $
      runParser (stream "lots") "lot of values" `shouldBe` Nothing
    it "test big stream" $
      runParser (stream "lot") "l" `shouldBe` Nothing
  describe "combinators combinations" $ do
    it "check 2 elements" $
      runParser (element 'c' *> element 'd') "cdef" `shouldBe` Just ('d', "ef")
    it "check 2 elements backwards" $
      runParser (element 'c' <* element 'd') "cdef" `shouldBe` Just ('c', "ef")
    it "check 1 element" $
      runParser (element 'c' <|> element 'd') "cdef" `shouldBe` Just ('c', "def")
    it "check only one sym" $
      runParser (element 'c' <* eof) "c" `shouldBe` Just ('c',"")
    it "check only one sym" $
      runParser (element 'c' <* eof) "cc" `shouldBe` Nothing
  describe "parsers check" $ do
    it "good brackets check" $
      runParser trueBrackets "(()(()))()" `shouldSatisfy` isJust
    it "bad brackets check" $
      runParser trueBrackets "((()()()))))" `shouldSatisfy` isNothing
    it "empty brackets check" $
      runParser trueBrackets "" `shouldSatisfy` isJust
    it "one bracket" $
      runParser trueBrackets "(" `shouldSatisfy` isNothing
    it "good int parse" $
      runParser int "123 left_part" `shouldBe` Just (123, " left_part")
    it "bad string parse" $
      runParser int "not a number" `shouldBe` Nothing
    it "parse +100" $
      runParser int "+100" `shouldBe` Just (100, "")
    it "parse -100" $
      runParser int "-100" `shouldBe` Just (-100, "")
    it "parse +-100" $
      runParser int "+-100" `shouldBe` Nothing
    it "parse empty string" $
      runParser int "" `shouldBe` Nothing
    it "lonely -" $
      runParser int "-one" `shouldBe` Nothing



main :: IO ()
main = hspecTestTree >>= \unitTests ->
             let allTests = testGroup "PARSERS" [unitTests]
             in defaultMain allTests
