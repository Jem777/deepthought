module Compile where

import Types
import Parser

getFunctionNames :: Tree -> Maybe [String] 
getFunctionNames = (f []) . (map (atomName . funcName)) . treeFuncs 
        where
        f outp [] = Just (reverse outp)
        f (y:ys) (x:xs) | elem x ys = Nothing
                        |  x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs
