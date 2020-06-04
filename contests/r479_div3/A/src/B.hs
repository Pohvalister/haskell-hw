module Main where

import Control.Monad.Reader

main :: IO ()
main = do
  numS <- getLine
  --let num = (read :: String -> Int) numsS
  str <- getLine
  putStr $ solve [] (str)

solve :: [(Int, String)] ->  String -> String
solve env str = let (_, answ) = (runReader (solveR str) env) in
  answ
solveR :: String -> Reader ([(Int,String)]) (Int,String)
solveR (c1 : c2 : cs) = local (addPair c1 c2) (solveR (c2 : cs))
  where
    addPair :: Char -> Char -> [(Int, String)] -> [(Int, String)]
    addPair c1 c2 ((i,s) : ps) = if (s) == (c1:[c2])
                                then ((i+1,s):ps)
                                else ((i,s): (addPair c1 c2 ps))
    addPair c1 c2 [] = [(1, c1:[c2])]
solveR (c:[]) = asks (getMost)
  where
    getMost :: [(Int, String)] -> (Int, String)
    getMost lst = foldl (\(io, so) (in1, sn) ->  if io < in1
                                      then (in1, sn)
                                      else (io, so)
                        ) (0,"") lst
