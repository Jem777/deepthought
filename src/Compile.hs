module Compile where

import Types
import Parser

import Data.List
import Data.Either


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

--first argument are global funs and operators, second list of local functions
getFunctionNames :: [Expression] -> [Expression] -> Either CompileError [Expression]
getFunctionNames l = (f l) . (map funcName)
        where
        f outp [] = Right (reverse outp)
        f [] (x:xs) = f [x] xs
        f (y:ys) (x:xs) | elem x ys = Left (CompileError "NameError" (position x) "foobar")
                        | x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs 

checkVars :: Expression -> Either [CompileError] [Expression]
checkVars f = getVars [] f

unusedVars :: [Expression] -> Expression -> Either [CompileError] [Expression]
unusedVars allowed exp = unused (rightSide leftSide)
        where 
        leftSide = varArgs allowed exp
        rightSide (Right x) = getVars x (body exp)
        rightSide (Left x) = Left x
        unused (Right x) = Right $ filterUnused (right leftSide) x
        unused (Left x) = Left x

-- getVars allowedVars expression = usedVars
getVars :: [Expression] -> Expression -> Either [CompileError] [Expression]
getVars allowed exp 
        | (isVar exp) && (elem exp allowed) = Right [exp]
        | (isVar exp) = Left [CompileError "Variable unbound" (position exp) "barbaz"]
        | (isApp exp) = sFold $ (getVars allowed (appName exp)) : (map (\y -> getVars allowed y) (appArgs exp))
        | (isDatatype exp) && (isTupel (dataType exp)) = sFold (map (\y -> getVars allowed y) (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = sFold (map (\y -> getVars allowed y) (listValue (dataType exp)))
        | (isLambda exp) = unusedVars allowed exp 
        | (isFunction exp) = g (unusedVars allowed exp) (eitherFold xor (++) $ map (unusedVars leftSide) (funcWhere exp))
        | otherwise = Right []
        where
        g (Left x) _ = Left x
        g _ (Left x) = Left x
        g (Right x) (Right y) = Right (xor x y)
        leftSide = right (varArgs allowed exp)

varArgs allowed exp = f (sFold (map (getVarArgs allowed) (args exp))) allowed
        where
        f (Right x) y = Right (union x y)
        f (Left x) _ = Left x 

getVarArgs :: [Expression] -> Expression -> Either [CompileError] [Expression]
getVarArgs allowed exp
        | (isVar exp) && (elem exp allowed) = Left [CompileError "Conflicting Definitions" (position exp) "barbaz"]
        | (isVar exp) = Right [exp]
        | (isApp exp) = sFold (map (\y -> getVarArgs allowed y) (appArgs exp))
        | (isDatatype exp) && (isTupel (dataType exp)) = sFold (map (\y -> getVarArgs allowed y) (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = sFold (map (\y -> getVarArgs allowed y) (listValue (dataType exp)))
        | otherwise = Right []


-- internal functions

equal x y = any (\z -> elem z x) y

filterUnused l r = filter (\y -> notElem y r) l

posX :: (Eq a) => [a] -> [a] -> a
posX x y = head (filter (\z -> elem z y) x)
posY x y = head (filter (\z -> elem z x) y)

--xor a b = (a ++ b) \\ (intersect a b)
xor = (\\) 

filtermap _ _ [] = []
filtermap f m (x:xs) | f x == True = (m x) : (filtermap f m xs)
                     | otherwise = filtermap f m xs


sFold :: (Eq a) => [Either [a1] [a]] -> Either [a1] [a]
sFold = eitherFold union (++)

eitherFold _ _ [] = Right []
eitherFold f g l 
        | (not . null) (lefts l) = Left (foldl1 g (lefts l))
        | otherwise = Right (foldl1 f (rights l))

right (Right x) = x
left (Left x) = x

testing = Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Fun testEmptyPos "y"]) [Function testEmptyPos (Fun testEmptyPos "y") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "W",Datatype testEmptyPos (Number 2)]) []]
