Module Main where

import Data.List
main :: IO ()
main = do
  numsS <- getLine
  let nums = map (read :: String -> Int) $ words numsS
  valsS <- getLine
  let vals = map (read :: String -> Int) $ words valsS
  print $ solve (nums!!1) $ sort vals

solve :: Int -> [Int] -> Int
solve 0 (1:_) = -1
solve 0 lst = 1

solve k lst | (length lst) == k = last lst
            | otherwise =
                let (fstL, sndL) = splitAt k lst in
                let (fstV, sndV) = (last fstL, head sndL) in
                if (fstV == sndV)
                then -1
                else fstV
