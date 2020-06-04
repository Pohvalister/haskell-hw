module Main where

main :: IO ()
main = do
  numsS <- getLine
  valsS <-getLine
  let vals = map (read :: String -> Int) $ words valsS
  print $ solve 0 vals

solve :: Int -> [Int] -> (Int, [Int])
solve ind lst =
