module Main where

main :: IO ()
main = do
  numsS <- getLine
  let nums = map (read :: String -> Int) $ words numsS
  print $ solve (head nums) (nums!!1)

solve :: Int -> Int -> Int
solve n k | k == 0 = n
          | n `mod` 10 == 0 = solve (n `div` 10) (k - 1)
          | otherwise = solve (n-1) (k-1)
