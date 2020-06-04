module Main where

import Data.List (find)

colors = [("red","Reality"), ("purple","Power"), ("yellow","Mind"), ("orange","Soul"), ("blue","Space"), ("green","Time")]

main :: IO ()
main = do
  x <- getLine
  lst <- readWords ((read :: String -> Int) x)
  print ((length colors) - (length lst))
  solveW lst

readWords :: Int -> IO [String]
readWords 0 = return []
readWords n = do
  lst <- readWords(n-1)
  name <- getLine
  return (name : lst)


solveW :: [String] -> IO ()
solveW lst = foldl (\a (b,name) -> case find (== b) lst of
                                      Just _  ->  a
                                      Nothing -> do putStrLn name
                                                    a
                  ) (return ())  colors
