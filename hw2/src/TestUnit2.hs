module TestUnit2 where

import Test.Tasty (TestTree)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Data.Maybe (isJust, isNothing)

import Unit2 (stringSum)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "stringSum_test" specSumm

specSumm :: Spec
specSumm = do
  describe "check parsing" $ do
    it "check correct positive string" $
      stringSum "2 3 4 10 0" `shouldSatisfy` isJust
    it "check correct string" $
      stringSum "3 -2 -6 10 0 -1" `shouldSatisfy` isJust
    it "check correct sum of string" $
      stringSum "3 -2 -6 10 0 -1" `shouldBe` Just 4
    it "check big sum of string" $
      stringSum "-1  -1  -1  -1  100000000 -1 -1 -1 -1 1 -1 -1 1 1 1 -1 -1" `shouldBe` Just 99999992
  describe "check failing" $ do
    it "check symbols" $
      stringSum "a" `shouldSatisfy` isNothing
    it "check typo" $
      stringSum "1 -1 1 -1 1- 1" `shouldBe` Nothing
    it "check typo2" $
      stringSum "100 - 1" `shouldBe` Nothing
    it "check big string" $
      stringSum " 1 1 1 1 1 1 1 1 1 11 1 1 11 1 1 1 11 1  1 11111 1 1 1 1111 -2 / 1" `shouldSatisfy` isNothing

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "STRINGSUM" [unitTests]
       in defaultMain allTests

--task2 - доказательство

-- instance Monad Optional where
--     return :: a -> Optional a
--     return val = Optional (Just (Just val))
--
--     (>>=) :: Optional a -> (a -> Optional b) -> Optional b
--     (>>=) val func = case val of
--       Optional Nothing -> Optional Nothing
--       Optional (Just Nothing) -> Optional (Just Nothing)
--       Optional (Just (Just x)) -> func x

-- LAW: return a >>= f === f a
--
-- return a >>= f =/раскроем_return/= Optional (Just (Just val)) >>= f =/по_3_ему_типу_(>>=)/= f a
--
-- LAW: m >>= return === m
--
-- при m = Optional Nothing
-- m >>= return  =/подставим_m_и_по_1_типу_(>>=)/= Optional Nothing === m
--
-- при m = Optional (Just Nothing)
-- m >>= return  =/подставим_m_и_по_2_типу_(>>=)/= Optional (Just Nothing) === m
--
-- при m = Optional (Just Just val)
-- m >>= return  =/подставим_m_и_по_3_типу_(>>=)/= return val  =/раскроем_по_return/=  Optional (Just Just val)
--
-- LAW: (m >>= f) >>= g ==== m >>= (\x -> f x >>= g)
--
-- при m = Optional Nothing  -- можно пойти по двум частям равенства сразу
-- ЛЧ: (m >>= f) >>= g =/подставим_m_и_по_1_типу_(>>=)/= Optional Nothing >>= g =/подставим_m_и_по_1_типу_(>>=)/= Optional Nothing
-- ПЧ: m >>= (\x -> f x >>= g)  =/подставим_m_и_по_1_типу_(>>=)/=  Optional Nothing
-- пришли к одному же выражению => можно по равенсвам дойти от одного до другого
--
-- при m = Optional Just Nothing  -- аналогично
-- ЛЧ: (m >>= f) >>= g =/подставим_m_и_по_2_типу_(>>=)/= Optional (Just Nothing) >>= g =/подставим_m_и_по_2_типу_(>>=)/= Optional (Just Nothing)
-- ПЧ: m >>= (\x -> f x >>= g)  =/подставим_m_и_по_2_типу_(>>=)/=  Optional (Just Nothing)
--
-- при m = Optional (Just (Just val))
-- ЛЧ: (m >>= f) >>= g =/подставим_m_и_по_3_типу_(>>=)/= f val >>= g
-- ПЧ: m >>= (\x -> f x >>= g)  =/подставим_m_и_по_3_типу_(>>=)/=  (\x -> f x >>= g) val  =/применим_лямбду/= f val >>= g
--
-- все случаи всех законов рассмотрены, левые и правые части сошлись => ЧТД
