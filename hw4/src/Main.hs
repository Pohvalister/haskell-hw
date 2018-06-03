module Main where

import Test.Tasty (TestTree)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)
import MainCode

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "test" specTest

-- to test HSPEC: runhaskell "this file"
specTest :: Spec
specTest = do
  describe "view tests" $ do
    it "view 1 similar types" $
      view _1 ("2", "3") `shouldBe` "2"
    it "view 1 different types" $
      view _1 ( 2 , "3") `shouldBe`  2
    it "view 2 similar types" $
      view _2 ( 2 ,  3 ) `shouldBe`  3
    it "view 2 different types" $
      view _2 ( 2 , "3") `shouldBe` "3"
  describe "over tests" $ do
    it "over 1 similar types" $
      over _1 (++"1") ("2", "3") `shouldBe` ("21", "3")
    it "over 1 different types" $
      over _1 (+1   ) ( 2 , "3") `shouldBe` ( 3  , "3")
    it "over 2 similar types" $
      over _2 (+1   ) ( 2 ,  3 ) `shouldBe` ( 2 ,  4 )
    it "over 2 different types" $
      over _2 ("1"++) ( 2 , "3") `shouldBe` ( 2 , "13")
  describe "set tests" $ do
    it "set 1 similar types" $
      set _1 "1" ("2", "3") `shouldBe` ("1", "3")
    it "set 1 different types" $
      set _1  1  ( 2 , "3") `shouldBe` ( 1 , "3")
    it "set 2 similar types" $
      set _2  1  ( 2 ,  3 ) `shouldBe` ( 2 ,  1 )
    it "set 2 different types" $
      set _2 "1" ( 2 , "3") `shouldBe` ( 2 , "1")
  describe "check laws" $ do
    it "you get back what you put in 1" $
      view _1 (set _1 "1" ("2", "3")) `shouldBe` "1"
    it "you get back what you put in 2" $
      view _2 (set _2  1  ( 2 ,  3 )) `shouldBe`  1
    it "putting back what you got doesn't change anything 1" $
      set _1 (view _1 ("2", "3")) ("2", "3") `shouldBe` ("2", "3")
    it "putting back what you got doesn't change anything 2" $
      set _2 (view _2 ( 2 ,  3 )) ( 2 ,  3 ) `shouldBe` ( 2 ,  3 )
    it "setting twice is the same as setting once 1" $
      set _1 "1" (set _1 "0" ("2", "3")) `shouldBe`  ("1", "3")
    it "setting twice is the same as setting once 2" $
      set _2  1  (set _2  0  ( 2 ,  3 )) `shouldBe`  ( 2 ,  1 )


main :: IO ()
main = hspecTestTree >>= \unitTests ->
            let allTests = testGroup "TEST" [unitTests]
            in defaultMain allTests
