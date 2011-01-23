module Eval (eval, saveArgs)where

import AST
import StdLib
import ASTErrors

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative ((<$>))
import Control.Monad

--eval (Application (Lambda l) pos args) = l pos args
eval (Application op pos args) = eval op >>= \(Lambda operator) -> operator pos args -- \(Lambda o) -> mapM eval args >>= \a -> o pos a
eval (Operator pos m f) = callFunction (m,f)
eval (Variable pos name) = resolveVariable name
eval (List list) = List <$> mapM eval list
eval (Vector list) = Vector <$> mapM eval list
eval x = goRight x
{-
data ASTDatatype =
    List [ASTDatatype]
    | Vector [ASTDatatype]
    | Number Integer
    | Float Double
    | String String
    | Char Char
    | Atom String
    | Operator SourcePos String String
    | Variable SourcePos String
    | Application ASTDatatype SourcePos [ASTDatatype]
    | Lambda (SourcePos -> [ASTDatatype] -> Eval ASTDatatype)
-}

getTree (EvalState t _) = t
getVariables (EvalState _ v) = v
changeTree f (EvalState t v) = EvalState (f t) v
changeVariables f (EvalState t v) = EvalState t (f v)

callFunction = call (\(m,f) -> (=<<) (Map.lookup f) . Map.lookup m . getTree) BlubbError
resolveVariable = call (\v -> Map.lookup v . getVariables) BlubbError
addVariable key value = modify (changeVariables (Map.insert key value))

maybeError f = EvalMonad . return . maybe (Left [f]) Right
call f g a = reduceEvalMonad . gets $ maybeError g . (f a)

matchingArgs args pattern
    | length pattern == length args = Right (zip args pattern)
    | otherwise = Left [BlubbError]

merge :: (ASTDatatype, ASTDatatype) -> Either [RuntimeError] [(String, ASTDatatype)]
merge (Atom "__wildcard__", _) = return []
merge (Variable _ name, a) = return [(name, a)]
merge (Application (Operator _ "StdLib" ":") _ args, List a)
    | length args == 1 + length a = fmap concat (mapM merge (zip args a))
    | length args <= length a = (fmap concat . mapM merge) ((last args, List (drop (length a - length args) a)):(zip (init args) a))
    | otherwise = Left [BlubbError]
merge (a, b) = if a == b then return [] else Left [BlubbError]

getArgs args pattern = matchingArgs args pattern >>= mapM merge >>= return . concat
saveArgs a = (>>= mapM (uncurry addVariable)) . reduceEvalMonad . return . EvalMonad . return . getArgs a
{-
merge :: (Types.Expression, ASTDatatype) -> Either [CompilerError] [(String, ASTDatatype)]
merge (Types.Wildcard, _) = return []
merge (Types.Variable _ name, a) = return [(name, a)]
merge (Types.Application _ (Types.Operator _ "" ":") args, List a)
    | length args == 1 + length a = fmap concat (mapM merge (zip args a))
    | length args <= length a = (fmap concat . mapM merge) ((last args, List (drop (length a - length args) a)):(zip (init args) a))
    | otherwise = Left [FoobarError]
--merge (a, b) = toAst a == b
merge (args, pattern) = return []

getArgs pattern args = matchingArgs pattern args >>= mapM merge >>= return . concat
saveArgs a = (>> return ()) . (>>= mapM (uncurry addVariable)) . reduceMonad . return . CompilerMonad . getArgs a
-}

instance Monad (Either a) where
    return = Right
    x >>= f = (either Left f x)
