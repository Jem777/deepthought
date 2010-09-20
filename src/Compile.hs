module Compile where

import Types
import Parser
import Errors

import Data.List
import Data.Either
import Misc


-- TODOs:
-- move errors to Errors.hs
-- simplify functions

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
getFunctionNames :: [Expression] -> [Expression] -> Either [CompileError] [Expression]
getFunctionNames l = (f l) . (map funcName)
        where
        f outp [] = Right (reverse outp)
        f [] (x:xs) = f [x] xs
        f (y:ys) (x:xs) | elem x ys = Left [CompileError "NameError" (position x) "Conflicting definition with "]
                        | x == y = f (y:ys) xs
                        | otherwise = f (x:y:ys) xs 


---------------------
-- check Variables --
---------------------

checkVars :: Expression -> Either [CompileError] [Expression]
checkVars = unusedVars . (getVars [])

usedVars :: [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
usedVars allowed exp = used leftSide rightSide
        where 
        leftSide = varArgs allowed exp
        rightSide x = getVars x (body exp)
        used left right = either Left (\l -> f l (right l)) left
        f left = eitherRight (\(r1,r2) -> (union left r1,r2))
        --concats the left side with the varArgs of the body

-- getVars allowedVars expression = usedVars
getVars :: [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
getVars allowed exp 
        | (isVar exp) && (elem exp allowed) = Right ([], [exp])
        | (isVar exp) = Left [CompileError "Variable unbound" (position exp) "barbaz"]
        | (isApp exp) = uFold $ (getVars allowed (appName exp)) : (map (\y -> getVars allowed y) (appArgs exp))
        | (isDatatype exp) && (isVector (dataType exp)) = uFold (map (\y -> getVars allowed y) (vectorValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = uFold (map (\y -> getVars allowed y) (listValue (dataType exp)))
        | (isDatatype exp) && (isLambda (dataType exp)) = usedVars allowed exp
        | (isFunction exp) = g (usedVars allowed exp) (map (usedVars leftSide) (funcWhere exp))
        | otherwise = Right ([], [])
        where
        g x xs = either Left (const (uFold (x:xs))) x
        leftSide = right (varArgs allowed exp) --TODO: test this (right is unsafe)

unusedVars x = x >>= return . uncurry filterUnused

varArgs allowed exp = (\a b -> eitherRight (flip union b) a) (sFold (map (getVarArgs allowed) (args exp))) allowed

getVarArgs :: [Expression] -> Expression -> Either [CompileError] [Expression]
getVarArgs allowed exp
        | (isVar exp) && (elem exp allowed) = Left [CompileError "Conflicting Definitions" (position exp) "barbaz"]
        | (isVar exp) = Right [exp]
        | (isApp exp) = sFold (map (\y -> getVarArgs allowed y) (appArgs exp))
        | (isDatatype exp) && (isVector (dataType exp)) = sFold (map (\y -> getVarArgs allowed y) (vectorValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = sFold (map (\y -> getVarArgs allowed y) (listValue (dataType exp)))
        | otherwise = Right []

---------------------
-- check functions --
---------------------

checkFuncs = unusedFuncs []

unusedFuncs :: [Expression] -> [Expression] -> Either [CompileError] [Expression]
unusedFuncs imports funcs = (filt . uFold) (map (getFunc allowed) funcs)
        where
        filt = eitherRight (\(a,b) -> filterUnused a b)
        allowed = allowedFuncs imports funcs

getFunc :: Either [CompileError] [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
getFunc (Left x) _ = Left x
getFunc (Right a) exp = uFold ((usedFunc allowed (body exp)) : (map (getFunc allowed) (funcWhere exp)))
        where
        allowed = allowedFuncs a (funcWhere exp)
{-
getFunc2 :: [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
getFunc2 a exp = mapM (getFunc2 allowed) (funcWhere exp)
        where
        allowed = allowedFuncs a (funcWhere exp)
-}

usedFunc :: Either [CompileError] [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
usedFunc (Left x) _ = Left x
usedFunc (Right allowed) exp
        | (isApp exp) = uFold $ (recursive (appName exp)) : (map recursive (appArgs exp))
        | (isOp exp) && (elem exp allowed) = Right (allowed, [exp])
        | (isOp exp) = Left [CompileError "NameError" (position exp) ("function '" ++ (name exp) ++ "' not defined")]
        | (isDatatype exp) && (isVector (dataType exp)) = uFold (map recursive (vectorValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = uFold (map recursive (listValue (dataType exp)))
        | (isDatatype exp) && (isLambda (dataType exp)) = recursive (lambdaBody (dataType exp))
        | otherwise = Right (allowed, [])
        where
        recursive = usedFunc (Right allowed)

allowedFuncs a = eitherRight (union a) . getFunctionNames a

----------------
-- check tree --
----------------

-- takes a parsetree and checks exports - import-check not implemented yet -> []
checkExports2 tree = getFunctionNames [] (treeFuncs tree) >>= checkExports tree

checkExports :: Tree -> [Expression] -> Either [CompileError] [Expression]
checkExports x y
        | isInfixOf y (treeExports x) = Left [CompileError "ExportError" (testEmptyPos) "exported a non existing function"]
        | otherwise = Right $ filterUnused (treeExports x) y

checkImports = True


-- internal functions

filterUnused l r = filter (\y -> notElem y r) l

posX :: (Eq a) => [a] -> [a] -> a
posX x y = head (filter (\z -> elem z y) x)
posY x y = head (filter (\z -> elem z x) y)

filtermap _ _ [] = []
filtermap f m (x:xs) | f x == True = (m x) : (filtermap f m xs)
                     | otherwise = filtermap f m xs

uFold [] = Right ([], [])
uFold l
        | (not . null) (lefts l) = Left (foldl1 (++) (lefts l))
        | otherwise = Right (foldl1 union (fst x), foldl1 union (snd x))--(foldl1 first r, foldl1 second r)
        where 
        x = unzip (rights l)

sFold = eitherFold (++) union

right (Right x) = x
left (Left x) = x

testing = [Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Variable testEmptyPos "X") []]
