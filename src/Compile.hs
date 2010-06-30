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
        used (Left l) _ = Left l
        used (Right l) r = f l (r l)
        f _ (Left r) = Left r
        f l (Right (r1,r2)) = Right (union l r1,r2) --concats the left side with the varArgs of the body

-- getVars allowedVars expression = usedVars
getVars :: [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
getVars allowed exp 
        | (isVar exp) && (elem exp allowed) = Right ([], [exp])
        | (isVar exp) = Left [CompileError "Variable unbound" (position exp) "barbaz"]
        | (isApp exp) = uFold $ (getVars allowed (appName exp)) : (map (\y -> getVars allowed y) (appArgs exp))
        | (isDatatype exp) && (isTupel (dataType exp)) = uFold (map (\y -> getVars allowed y) (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = uFold (map (\y -> getVars allowed y) (listValue (dataType exp)))
        | (isLambda exp) = usedVars allowed exp
        | (isFunction exp) = g (usedVars allowed exp) (map (usedVars leftSide) (funcWhere exp))
        | otherwise = Right ([], [])
        where
        g (Left x) _ = Left x
        g x xs = uFold (x:xs)
        leftSide = right (varArgs allowed exp) --TODO: test this (right is unsafe)

unusedVars :: Either [CompileError] ([Expression], [Expression]) -> Either [CompileError] [Expression]
unusedVars (Left x) = Left x
unusedVars (Right (allowed, used)) = Right (filterUnused allowed used)

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

---------------------
-- check functions --
---------------------

checkFuncs = unusedFuncs (Right [])

unusedFuncs (Left x) _ = Left x --probably not needed
unusedFuncs (Right imports) funcs = (filt . uFold) (map (getFunc allowed) funcs)
        where
        filt (Left x) = Left x
        filt (Right (a,b)) = Right (filterUnused a b)
        allowed = allowedFuncs imports funcs

getFunc :: Either [CompileError] [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
getFunc (Left x) _ = Left x
getFunc (Right a) exp = uFold ((usedFunc allowed (body exp)) : (map (getFunc allowed) (funcWhere exp)))
        where
        allowed = allowedFuncs a (funcWhere exp)

usedFunc :: Either [CompileError] [Expression] -> Expression -> Either [CompileError] ([Expression], [Expression])
usedFunc (Left x) _ = Left x
usedFunc (Right allowed) exp
        | (isApp exp) = uFold $ (recursive (appName exp)) : (map recursive (appArgs exp))
        | ((isFun exp) || (isOp exp)) && (elem exp allowed) = Right (allowed, [exp])
        | ((isFun exp) || (isOp exp)) = Left [CompileError "NameError" (position exp) ("function '" ++ (value exp) ++ "' not defined")]
        | (isDatatype exp) && (isTupel (dataType exp)) = uFold (map recursive (tupelValue (dataType exp)))
        | (isDatatype exp) && (isList (dataType exp)) = uFold (map recursive (listValue (dataType exp)))
        | (isLambda exp) = recursive (body exp)
        | otherwise = Right (allowed, [])
        where
        recursive = usedFunc (Right allowed)

allowedFuncs a exp = allowed funcNames
        where
        funcNames = getFunctionNames a exp
        allowed (Left x) = Left x
        allowed (Right x) = Right (union a x)

----------------
-- check tree --
----------------

-- takes a parsetree and checks exports - import-check not implemented yet -> []
checkExports2 tree = checkExports tree (getFunctionNames [] (treeFuncs tree))

checkExports :: Tree -> Either [CompileError] [Expression] -> Either [CompileError] [Expression]
checkExports _ (Left y) = Left y
checkExports x (Right y) 
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

sFold :: (Eq a) => [Either [a1] [a]] -> Either [a1] [a]
sFold [] = Right []
sFold l 
        | (not . null) (lefts l) = Left (foldl1 (++) (lefts l))
        | otherwise = Right (foldl1 union (rights l))

right (Right x) = x
left (Left x) = x

testing = [Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Variable testEmptyPos "X") []]
