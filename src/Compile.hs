module Compile where

import Types
import Parser

import Data.List

-- this module checks whether the semantics are correct
--
-- all variables and functions bound
-- return warning on unused functions/variables
-- all exported functions in the module
-- no double assignment of functions/variables

parse fname g = parser fname >>= return . f
    where 
        f (Left err) = show err
        f (Right x) = show (g x) 

--probably it later changes to [Expression] -> [Expression] -> Either CompileError [Expression]
--because it handles functions not just strings
--first argument are global funs and operators, second list of local functions
getFunctionNames :: [Expression] -> [Expression] -> Either CompileError [Expression]
getFunctionNames l = (f l) . (map funcName)
        where
        f outp [] = Right (reverse outp)
        f [] (x:xs) = f [x] xs
        f (y:ys) (x:xs) | elem x ys = Left (CompileError "NameError" (position x) "foobar")
                        | x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs 

unusedVars :: Expression -> Either CompileError [Expression]
unusedVars func = v (foldVarArgs (map (getVarArgs []) (args func)))
        where 
        v (Right x) = (getVars x (body func))
        v (Left x) = Left x

-- getVars allowedVars expression = usedVars
getVars :: [Expression] -> Expression -> Either CompileError [Expression]
getVars allowed exp 
        | (isVar exp) && (elem exp allowed) = Right [exp]
        | (isVar exp) = Left (CompileError "Variable unbound" (position exp) "barbaz")
        | (isApp exp) = (eitherFold f) (map (\y -> getVars allowed y) (appArgs exp))
        | (isDatatype exp) && (isTupel (dataType exp)) = (eitherFold f) (map (\y -> getVars allowed y) (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = (eitherFold f) (map (\y -> getVars allowed y) (listValue (dataType exp)))
        -- lambdas and functions are missing
        | otherwise = Right []
        where
        f x y = Right (union x y)


foldVarArgs :: [Either CompileError [Expression]] -> Either CompileError [Expression]
foldVarArgs = eitherFold f
        where
        f x y | equal x y = Left (CompileError "Conflicting Definitions" (position (posX x y)) "barbaz")
              | otherwise = Right (x ++ y)


getVarArgs :: [Expression] -> Expression -> Either CompileError [Expression]
getVarArgs allowed exp
        | (isVar exp) && (elem exp allowed) = Left (CompileError "Conflicting Definitions" (position exp) "barbaz")
        | (isVar exp) = Right [exp]
        | (isApp exp) = (eitherFold f) (map (\y -> getVarArgs allowed y) (appArgs exp))
        | (isDatatype exp) && (isTupel (dataType exp)) = (eitherFold f) (map (\y -> getVars allowed y) (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = (eitherFold f) (map (\y -> getVars allowed y) (listValue (dataType exp)))
        | otherwise = Right []
        where
        f x y | equal x y = Left (CompileError "Conflicting Definitions" (position exp) "barbaz")
              | otherwise = Right (x ++ y)

-- inputs are: global functions (may change type to [Expression]), exports and expression to check
{-usedFunctions :: [String] -> [String] -> Expression -> Either CompileError [Expression]
usedFunctions glob exp expr 
            | (isApp expr) = 
            where
            f | elem (value (appName expr)) glob = 
            -- | (isVar expr) = Right []
            -- | (isFun expr) = (value expr)
-}
-- internal functions

equal x y = any (\z -> notElem z x) y

posX :: (Eq a) => [a] -> [a] -> a
posX x y = head (filter (\z -> elem z y) x)
posY x y = head (filter (\z -> elem z x) y)


filtermap _ _ [] = []
filtermap f m (x:xs) | f x == True = (m x) : (filtermap f m xs)
                     | otherwise = filtermap f m xs

eitherFold :: ([a] -> [a] -> Either a1 [a]) -> [Either a1 [a]] -> Either a1 [a]
eitherFold _ [] = Right []
eitherFold _ [x] = x
eitherFold _ ((Left x):_) = Left x
eitherFold _ (_:(Left x):_) = Left x
eitherFold f ((Right x):(Right y):xs) = eitherFold f ((f x y):xs)
