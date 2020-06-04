module Main where

main :: IO ()
main = do
  xy <-getLine
  let (x:[y]) = map (read :: String -> Int) $ words xy
  solve x y

solve :: Int -> Int -> IO ()
solve x y | x == y || (y == 2 && x == 4) || (x == 2 && y == 4) = putStr "="
          | x == 1 || (x == 2 && y == 3)                       = putStr "<"
          | y == 1 || (y == 2 && x == 3)                       = putStr ">"
          | x > y                                              = putStr "<"
          | y > x                                              = putStr ">"
