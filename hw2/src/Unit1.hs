module Unit1 where

import Control.Monad (liftM2)

--task1
data Expr = Const Int | Sum (Expr, Expr) | Sub (Expr, Expr) | Mul (Expr, Expr) | Div (Expr, Expr) | Pow (Expr, Expr)

newtype ArithmeticError = ArithmeticError String
  deriving (Show, Eq)

eval:: Expr -> Either ArithmeticError Int
eval expr = case expr of
  Const v -> return v
  Sum (e1,e2) -> liftM2 (+) (eval e1) (eval e2)
  Sub (e1,e2) -> liftM2 (-) (eval e1) (eval e2)
  Mul (e1,e2) -> liftM2 (*) (eval e1) (eval e2)
  Div (e1,e2) -> (>>=) (eval e2) divExp <*> eval e1
    where
      divExp :: Int -> Either ArithmeticError (Int -> Int)
      divExp dExp =  if dExp==0
        then Left $ ArithmeticError "Division on 0"
        else Right (div `flip` dExp)
  Pow (e1,e2) -> (>>=) (eval e2) powExp <*> eval e1
    where
      powExp :: Int -> Either ArithmeticError (Int -> Int)
      powExp pExp = if pExp>0
        then Right (^ pExp)
        else Left $ ArithmeticError "Negative powering"

--task2
bin :: Int -> [[Int]]
bin val
    | val < 1 = []
    | val == 1 = [[0],[1]]
    | otherwise =  bin (val - 1) >>= (\xs -> [0:xs,1:xs])
