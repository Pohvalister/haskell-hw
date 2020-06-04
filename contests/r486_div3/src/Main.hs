module Main where

import Data.List (sortBy)
import Control.Monad
import Data.Functor
import qualified Data.ByteString as B

main :: IO ()
main = do
  nS <-getLine
  let n = read nS :: Int
  strs <- replicateM n B.getLine
  case solve strs of
    Just answs -> do putStrLn "YES"
                     mapM_ B.putStrLn answs
                     return ()
    Nothing -> putStrLn "NO"


solve :: [B.ByteString] -> Maybe [B.ByteString]
solve strs = checkCorrectness (sortBy (\(a,_) (b,_) -> compare a b) (conf strs))--solve strs = checkCorrectness (sortOn fst (conf strs))
  where
    conf :: [B.ByteString] -> [(Int, B.ByteString)]
    conf = map (\str -> (B.length str, str))

    checkCorrectness :: [(Int, B.ByteString)] -> Maybe [B.ByteString]
    checkCorrectness [(_,lastPair)] = Just [lastPair]
    checkCorrectness ((int1, str1):(int2, str2):pairs) = checkCorrectness ((int2, str2):pairs) >>= (\prev -> str1:prev <$ B.findSubstring str1 str2)
