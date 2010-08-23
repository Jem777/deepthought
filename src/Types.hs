module Types
    where

import Data.List
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Pos as P

-- definition of all necessary types for parsing
-- and types for an intermediate byte-code
--
-- TODOs:
-- support for qualified operators and functions
-- add type RuntimeException

data Datatype = --primitve datatypes and lists and Vectors
            List [Expression]
            | Vector [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Atom String
            | Lambda [Expression] Expression -- [Expr] are the arguments, Expr is the Body -- is a datatype
            deriving (Eq)

data Expression = -- everything that evals to an datatype
            Variable {
                position :: SourcePos,
                varName :: String}
            | Operator {
                position :: SourcePos,
                value :: String}
            | Application {
                position :: SourcePos,
                appName :: Expression,
                appArgs :: [Expression]}
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Function {
                position :: SourcePos,
                funcName :: Expression,
                funcArgs :: [Expression],
                funcGuard :: Expression,
                funcBody :: Expression,
                funcWhere ::[Expression]}
            --first is the ident, second the args, third the guard, fourth the body, fifth closures
            | Datatype {
                position :: SourcePos,
                dataType :: Datatype}
            | Wildcard
            deriving (Show)

data Definition = Definition SourcePos String [([Expression], Expression, Expression, [Definition])]

data TreeObject = Expression (Integer, Integer) String Expression

data Tree = Tree String [String] [Expression] [([String], String)] [Expression] -- modname, compileflags, exports, imports, functions
            deriving (Show, Eq)

data CompileError = CompileError String SourcePos String --kind of Error, SourcePos, Message

data State =
    State [(String, Tree)] [(String, Definition)] [(String, Datatype)]
    -- arguments are imported modules, functions and variables

type SourcePos = P.SourcePos

newtype (Monad m) => EitherErr m a = EitherErr { runEitherErr :: m (Either [CompileError] a)}
newtype EitherList a = EitherList {runEitherList :: Either [CompileError] [a]}

-- instances for the types
instance Monad (Either a) where
    return = Right
    x >>= f = either Left f x


instance Monad EitherList where
    return = EitherList . Right . (:[])
    --x >>= f = EitherList (either Left (runEitherList . map . f) (runEitherList x))
    x >>= f = EitherList (either Left (runEitherList . f . head) (runEitherList x))

instance Functor EitherList where
    fmap f x = EitherList (either Left (Right . (map f)) (runEitherList x))

instance (Monad m) => Monad (EitherErr m) where
    return = EitherErr . return . Right
    x >>= f = EitherErr (runEitherErr x >>= either (return . Left) (runEitherErr . f))

instance (Monad m) => Functor (EitherErr m) where
    fmap f x = EitherErr (runEitherErr x >>= either (return . Left) (return . Right . f))

instance (Monad m) => MonadPlus (EitherErr m) where
    mzero = (EitherErr . return . Left) []
    mplus x y = EitherErr ((\a -> either (\b -> either (Left . (++b)) Right a) Right) `liftM` runEitherErr y `ap` runEitherErr x)

instance Show Datatype where
    show (Number a) = show a
    show (Float a) = show a
    show (String a) = show a
    show (Char a) = show a
    show (Atom a) = a


instance Show CompileError where
    show (CompileError a b c) = a ++ " at " ++ show b ++ ":\n" ++ c

instance Eq CompileError where
    (CompileError a _ c) == (CompileError a' _ c') = a == a'

instance Eq Expression where
    (Variable _ a) == (Variable _ b) = a == b
    (Operator _ a) == (Operator _ b) = a == b
    (Application _ a b) == (Application _ a' b') = a == a' && b == b'
    (Function _ a b c d e) == (Function _ a' b' c' d' e') = a == a' && b == b' && c == c' && d == d' && e == e'
    (Datatype _ a) == (Datatype _ b) = a == b
    Wildcard == Wildcard = True
    _ == _ = False


-- a lot of trivial functions for using the types

testEmptyPos = P.newPos "" 0 0
testEmptyState = State [] [] []

-- functions for the state

getVariable (State _ _ a) b = lookup b a
getFunction (State _ a _) b = lookup b a
getTree (State a _ _) b = lookup b a

-- functions for the tree

treeName (Tree a _ _ _ _) = a
treeCompile (Tree _ a _ _ _) = a
treeExports (Tree _ _ a _ _) = a
treeImports (Tree _ _ _ a _) = a
treeFuncs (Tree _ _ _ _ a) = a

lambdaArgs (Lambda a _) = a
lambdaBody (Lambda _ a) = a

isList (List _) = True
isList _ = False
isVector (Vector _) = True
isVector _ = False
isVar (Variable _ _) = True
isVar _ = False
isOp (Operator _ _) = True
isOp _ = False
isApp (Application _ _ _) = True
isApp _ = False
isLambda (Lambda _ _) = True
isLambda _ = False
isFunction (Function _ _ _ _ _ _) = True
isFunction _ = False
isDatatype (Datatype _ _) = True
isDatatype _ = False
isWildcard Wildcard = True
isWildcard _ = False

args (Function _ _ a _ _ _) = a
args (Datatype _ (Lambda a _)) = a
body (Function _ _ _ _ a _) = a
body (Datatype _ (Lambda _ a)) = a

listValue (List a) = a
vectorValue (Vector a) = a
