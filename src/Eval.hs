module Eval (eval, saveArgs) where

import AST
import ASTErrors
import Misc

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative ((<$>))
import Control.Monad

eval (Application op pos args) = statelessEval op >>= \(Lambda operator) -> mapM eval args >>= operator pos
eval (Operator pos m f) = callFunction (m,f)
eval (Variable pos name) = resolveVariable name
eval (List list) = List <$> mapM eval list
eval (Vector list) = Vector <$> mapM eval list
eval x = goRight x

statelessEval = StateT . flip (\s -> (>>= return . flip (,) s) . flip evalStateT s . eval)

getTree (EvalState t _) = t
getVariables (EvalState _ v) = v
changeTree f (EvalState t v) = EvalState (f t) v
changeVariables f (EvalState t v) = EvalState t (f v)

callFunction = call (\(m,f) -> (=<<) (Map.lookup f) . Map.lookup m . getTree) BlubbError
resolveVariable = call (\v -> Map.lookup v . getVariables) BlubbError
    -- both errors should not appear, otherwise there is a bug in the compiler
addVariable key value = modify (changeVariables (Map.insert key value))

maybeError f = EvalMonad . return . maybe (Left [f]) Right
call f g a = reduceEvalMonad . gets $ maybeError g . (f a)

matchingArgs pos args pattern
    | length pattern == length args = Right (zip args pattern)
    | otherwise = Left [PatternException pos (show args) (show pattern)]

merge :: Misc.SourcePos -> (ASTDatatype, ASTDatatype) -> Either [RuntimeError] [(String, ASTDatatype)]
merge _ (Atom "__wildcard__", _) = return []
merge _ (Variable _ name, a) = return [(name, a)]
merge pos (Application (Operator _ "StdLib" ":") _ args, List a)
    | length args == 1 + length a = concat <$> (mapM (merge pos) (zip args a))
    | length args <= length a = (fmap concat . mapM (merge pos)) ((last args, List (drop (length a - length args) a)):(zip (init args) a))
    | otherwise = Left [PatternException pos (show args) (show (List a))]
merge pos (List a, List b)
    | length a == length b = concat <$> mapM (merge pos) (zip a b)
    | otherwise = Left [PatternException pos (show (List a)) (show (List b))]
merge pos (Vector a, Vector b)
    | length a == length b = concat <$> mapM (merge pos) (zip a b)
    | otherwise = Left [PatternException pos (show (Vector a)) (show (Vector b))]
merge pos (a, b) = if a == b then return [] else Left [PatternException pos (show a) (show b)]

getArgs pos args pattern = matchingArgs pos args pattern >>= mapM (merge pos) >>= return . concat
saveArgs pos a = mapM (uncurry addVariable) <=< reduceEvalMonad . return . EvalMonad . return . getArgs pos a

instance Monad (Either a) where
    return = Right
    x >>= f = either Left f x
