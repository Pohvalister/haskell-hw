module TestUnit1 where

import Data.Either (isLeft, isRight)

import Test.Tasty (TestTree)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Unit1 (eval, bin, Expr(..), ArithmeticError(..))
--
import Hedgehog (Gen, Property, forAll, property, (===))
import Test.Tasty.Hedgehog (testProperty)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
---
hspecTestTree :: IO TestTree
hspecTestTree = testSpec "eval_test" specEval
-- to test HSPEC: runhaskell "this file"
specEval :: Spec
specEval = do
  describe "simple evals" $ do
    it "eval lone const" $
      eval (Const 1) `shouldSatisfy` isRight
    it "eval precise const" $
      eval (Const 10) `shouldBe` Right 10
    it "check Sum Sub Mul" $ -- (-1 + (5 * -13 - 4))==-70
      eval (Sum (Const (-1), Sub (Mul (Const 5, Const (-13)), Const 4))) `shouldBe` Right (-70)
  describe "div pow cases" $ do
    it "check Div" $
      eval (Div (Const 3, Const (-2))) `shouldBe` Right (-2)
    it "check Pow" $
      eval (Div (Pow (Const 10, Const 2), Const 2)) `shouldBe` Right 50
    it "check 0 divide Error" $
      eval (Div (Sum (Const 1,Const 1), Const 0)) `shouldSatisfy` isLeft
    it "check 0 divide Error - precise" $
      eval (Div (Sum (Const 1,Const 1), Const 0)) `shouldBe` Left (ArithmeticError "Division on 0")
    it "check pow negative" $
      eval (Sum (Pow (Const 10,Const (-10)), Const 0)) `shouldBe` Left (ArithmeticError "Negative powering")
    it "check failing tests" $
      eval (Const 1) `shouldSatisfy` isLeft
    it "check failing tests2" $
      eval (Div (Const 3, Const 0)) `shouldBe` Right 0


main :: IO ()
main = hspecTestTree >>= \unitTests ->
             let allTests = testGroup "EVAL" [unitTests]
             in defaultMain allTests

--
--propTestTree :: TestTree
--propTestTree = testProperty "bin always has 2^n values" prop_len

--genIntList :: Gen [Integer]
--genIntList =
--  let listLength = Range.Linear 0 10
--  in Gen.list listLength Gen.enumBounded

--prop_len :: Property
--prop_len = property $
--  forAll genIntList >>= \xs ->
--  List.length (bin xs) === ()
