module Compile where

import Types
import Parser

import Data.List

parse fname g = parser fname >>= return . f
    where 
        f (Left err) = show err
        f (Right x) = show (g x) 

getFunctionNames :: [Expression] -> Maybe [String]
getFunctionNames = (f []) . (map (header . funcName))
        where
        f outp [] = Just (reverse outp)
        f [] (x:xs) = f [x] xs
        f (y:ys) (x:xs) | elem x ys = Nothing
                        | x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs

variablesBound :: Expression -> [String] -> Bool
variablesBound func listOfGlobals = isInfixOf (f (funcBody func)) ((filtermap isVar varName (funcArgs func)) ++ listOfGlobals)
        where
        f _ = []



-- internal functions

filtermap _ _ [] = []
filtermap f m (x:xs) | f x == True = (m x) : (filtermap f m xs)
                     | otherwise = filtermap f m xs
