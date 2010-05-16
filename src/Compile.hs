module Compile where

import Types
import Parser

import Data.List

parse fname g = parser fname >>= return . f
    where 
        f (Left err) = show err
        f (Right x) = show (g x) 

--probably it later changes to [Expression] -> [Expression] -> Either CompileError [Expression]
--because it handles functions not just strings
--first argument are global funs and operators, second list of local functions
getFunctionNames :: [String] -> [Expression] -> Either CompileError [String]
getFunctionNames l = (f l) . (map funcName)
        where
        f outp [] = Right (reverse outp)
        f [] (x:xs) = f [value x] xs
        f (y:ys) (x:xs) | elem (value x) ys = Left (CompileError "NameError" (position x) "foobar")
                        | (value x) == y = f (y:ys) xs
                        | otherwise = f ((value x):y:ys) xs 

{-
variablesBound :: Expression -> [String] -> Bool
variablesBound func listOfGlobals = isInfixOf (f (funcBody func)) ((filtermap isVar varName (funcArgs func)) ++ listOfGlobals)
        where
        f _ = []

functionsBound :: Expression -> [String] -> Bool
functionsBound x y = False
-}

-- inputs are: global functions (may change type to [Expression]), exports and expression to check
usedFunctions :: [String] -> [String] -> Expression -> Either CompileError [Expression]
usedFunctions glob exp expr 
            | (isApp expr) = 
            where
            f | elem (value (appName expr)) glob = 
            -- | (isVar expr) = Right []
            -- | (isFun expr) = (value expr)

-- internal functions

filtermap _ _ [] = []
filtermap f m (x:xs) | f x == True = (m x) : (filtermap f m xs)
                     | otherwise = filtermap f m xs
