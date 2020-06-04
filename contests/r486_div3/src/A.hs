module Main where

import Data.List (find)

main :: IO ()
main = do
  nk <-getLine
  let (n:[k]) = map (read :: String -> Int) $ words nk
  numsS <- getLine
  let nums = map (read :: String -> Int) $ words numsS
  let (_, answLst, _) = solve n k nums
  if (length answLst == k)
  then do
   putStrLn "YES"
   foldl (\a val -> do putStr ((show  val) ++ " ")
                       a
          ) (return ()) answLst
  else
   putStr "NO"

solve :: Int -> Int -> [Int] -> ([Int], [Int], Int)
solve n k lst = foldl (\(newLst, newIter, i) val -> case find (== val) newLst of
                                          Just val -> (newLst, newIter, i+1)
                                          Nothing -> if ((length newLst) == k)
                                                   then (newLst, newIter, i+1)
                                                   else (val:newLst, i:newIter,  i+1)
                      ) ([],[], 1) lst
