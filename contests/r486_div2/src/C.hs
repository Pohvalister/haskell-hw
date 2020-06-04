module Main where

import Data.Traversable

infinite = 3 * 100000000 + 100

main :: IO ()
main = do
  n <- getLine
  fontsS <- getLine
  let fonts = map (read :: String -> Integer) $ words fontsS
  costsS <- getLine
  let costs = map (read :: String -> Integer) $ words costsS
  print $ solve fonts costs

solve :: [Integer] -> [Integer] -> Integer
solve (f1:f2:fs) (c1:c2:cs) = let answ = tryCost (f2, c2) [(f1,c1)] (zip fs cs) in
                              if answ == infinite
                              then -1
                              else answ

tryCost :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)] -> Integer
tryCost _ _ [] = infinite
tryCost (f,c) left right = min (c + fnd True f left + fnd False f right ) (tryCost (head right) ((f,c):left) (tail right))
  where
  fnd :: Bool -> Integer -> [(Integer, Integer)] -> Integer
  fnd is_l c_font = foldl (\minim (fo,co) -> if co < minim && ((fo < c_font) == is_l) && (fo /= c_font)
                                                 then co
                                                 else minim
                              ) infinite
