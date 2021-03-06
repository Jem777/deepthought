module AST where

import Control.Monad
import Control.Applicative
import Data.List (intercalate)
import Data.Map (Map)
import Control.Monad.State

import CompilerErrors
import ASTErrors
import Misc

type Eval = StateT EvalState EvalMonad
data EvalState = EvalState Tree Variables
newtype EvalMonad a = EvalMonad { runEvalMonad :: IO (Either [RuntimeError] a)}

type Compiler = StateT CompilerState CompilerMonad
data CompilerState = CompilerState Tree String Imports [String] [String]
newtype CompilerMonad a = CompilerMonad {runCompilerMonad :: Either [CompileError] a}

type Imports = Map String (String, [String]) --alias, module name and the functions
type Module = Map String ASTDatatype
type Tree = Map String Module
type Variables = Map String ASTDatatype

transferMonad = EvalMonad . return . (either (Left . map CompilerError) Right) . runCompilerMonad

reduceEvalMonad :: Eval (EvalMonad a) -> Eval a
reduceEvalMonad = StateT . (\f s -> f s >>= \(a, s) -> a >>= \x -> return (x,s)) . runStateT
doIO f x = toEval (f x >> (return . Right . Atom) "@ok")
goRight :: a -> Eval a
goRight = return
goLeft = toEval . return . Left
toEval = reduceEvalMonad . return . EvalMonad

getType (List _) = "List"
getType (Vector _) = "Vector"
getType (Number _) = "Number"
getType (Float _) = "Float"
getType (String _) = "String"
getType (Char _) = "Char"
getType (Atom _) = "Atom"
getType (Lambda _) = "Lambda"
getType (Operator _ _ _) = "Function"


instance Monad EvalMonad where
    return = EvalMonad . return . Right
    x >>= f = EvalMonad (runEvalMonad x >>= either (return . Left) (runEvalMonad . f))

instance MonadPlus EvalMonad where
    mzero = (EvalMonad . return . Left) []
    mplus x y = EvalMonad ((\a -> either (\b -> either (Left . (++b)) Right a) Right) `liftM` runEvalMonad y `ap` runEvalMonad x)

instance Functor EvalMonad where
    fmap f x = EvalMonad (runEvalMonad x >>= either (return . Left) (return . Right . f))

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


instance Eq ASTDatatype where
    (List a) == (List b) = a == b
    (Vector a) == (Vector b) = a == b
    (Number a) == (Number b) = a == b
    (Float a) == (Float b) = a == b
    (String a) == (String b) = a == b
    (Char a) == (Char b) = a == b
    (Atom a) == (Atom b) = a == b
    (Application a _ c) == (Application a' _ c') = a == a' && c == c'
    (Lambda _) == (Lambda _) = False
    _ == _ = False
instance Show ASTDatatype where
    show (List a) = show a
    show (Vector a) = "(" ++ (intercalate "," (map show a)) ++ ")"
    show (Number a) = show a
    show (Float a) = show a
    show (String a) = show a
    show (Char a) = show a
    show (Atom a) = a
    show (Lambda _) = "<Lambda>"
    show (Variable pos string) = string
    show (Application op _ args) = "Application " ++ show op ++ " to " ++ (concat . map show) args

isTrue (List []) = False
isTrue (Vector []) = False
isTrue (Number 0) = False
isTrue (Float 0) = False
isTrue (String "") = False
isTrue _ = True
