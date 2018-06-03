--{-# LANGUAGE TemplateHaskell #-}
module Main where

--------Unit1

import Language.Haskell.TH

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n lst = do let names = map (\x -> varE (mkName ("x" ++ show x))) lst
                           lamE [tupP (map (\x -> varP (mkName ("x" ++ show x))) [0..n-1])] (tupE names)
