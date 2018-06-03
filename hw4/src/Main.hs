module Main where

import Test.Tasty (TestTree)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)
import MainCode

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "test" specTest

specTest :: Spec
specTest = do
  describe "set tests" $ do
    it "set 1 similar types" $
      set _1 "1" ("2", "3") `shouldBe` ("1", "3")
    it "set 1 different types" $
      set _1  1  ( 2 , "3") `shouldBe` ( 1 , "3")
    it "set 2 similar types" $
      set _2  1  ( 2 ,  3 ) `shouldBe` ( 2 ,  1 )
    it "set 2 different types" $
      set _2 "1" ( 2 , "3") `shouldBe` ( 2 , "1")


main :: IO ()
main = hspecTestTree >>= \unitTests ->
            let allTests = testGroup "TEST" [unitTests]
            in defaultMain allTests
