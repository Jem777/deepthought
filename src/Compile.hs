module Compile where

import Types
import Parser

parse fname g = parser fname >>= return . f
    where 
        f (Left err) = show err
        f (Right x) = show (g x) 

getFunctionNames :: [Expression] -> Maybe [String] 
getFunctionNames = (f []) . (map (g . funcName))
        where
        g (Atom n) = n
        g (Operator n) = n
        f outp [] = Just (reverse outp)
        f [] (x:xs) = f [x] xs
        f (y:ys) (x:xs) | elem x ys = Nothing
                        | x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs
