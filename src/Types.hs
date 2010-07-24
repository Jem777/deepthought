module Types
    where

import qualified Text.ParserCombinators.Parsec.Pos as P

--
-- definition of all necessary types for parsing
-- and types for an intermediate byte-code
--
-- TODOs:
-- support for qualified operators and functions
-- add type RuntimeException

data Datatype = --primitve datatypes and lists and tupels
            List [Expression]
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Atom String
            | Lambda [Expression] Expression -- [Expr] are the arguments, Expr is the Body -- is a datatype
            deriving (Show, Eq)

data Expression = -- everything that evals to an datatype
            Variable SourcePos String
            | Fun SourcePos String
            | Operator SourcePos String
            | Application SourcePos Expression [Expression]
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Function SourcePos Expression [Expression] Expression Expression [Expression] 
            --first is the ident, second the args, third the guard, fourth the body, fifth closures
            | Datatype SourcePos Datatype
            | Wildcard
            deriving (Show)

data TreeObject = Expression (Integer, Integer) String Expression

data Tree = Tree String [String] [Expression] [([String], String)] [Expression] -- modname, compileflags, exports, imports, functions
            deriving (Show, Eq)

data CompileError = CompileError String SourcePos String --kind of Error, SourcePos, Message

data State =
    State [(String, Tree)] [(Expression, Expression)] [(Expression, Datatype)]
    -- arguemts are imported modules, functions and variables

type SourcePos = P.SourcePos
-- instances for the types -  SourcePos is irrelevant to equalency

instance Show CompileError where
    show (CompileError a b c) = a ++ " at " ++ show b ++ ":\n" ++ c

instance Eq CompileError where
    (CompileError a _ c) == (CompileError a' _ c') = a == a'

instance Eq Expression where
    (Variable _ a) == (Variable _ b) = a == b
    (Fun _ a) == (Fun _ b) = a == b
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

addToState :: State -> [Expression] -> [Expression] -> State
addToState state pattern arguments -> state

-- functions for the tree

treeName (Tree a _ _ _ _) = a
treeCompile (Tree _ a _ _ _) = a
treeExports (Tree _ _ a _ _) = a
treeImports (Tree _ _ _ a _) = a
treeFuncs (Tree _ _ _ _ a) = a

varName (Variable _ a) = a
appName (Application _ a _) = a
appArgs (Application _ _ a) = a
lambdaArgs (Lambda a _) = a
lambdaBody (Lambda _ a) = a
funcName (Function _ a _ _ _ _) = a
funcArgs (Function _ _ a _ _ _) = a
funcGuard (Function _ _ _ a _ _) = a
funcBody (Function _ _ _ _ a _) = a
funcWhere (Function _ _ _ _ _ a) = a
opName (Operator _ a) = a
funName (Fun _ a) = a
dataType (Datatype _ a) = a

isList (List _) = True
isList _ = False
isTupel (Tupel _) = True
isTupel _ = False
isVar (Variable _ _) = True
isVar _ = False
isFun (Fun _ _) = True
isFun _ = False
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
tupelValue (Tupel a) = a

value (Operator _ a) = a
value (Fun _ a) = a

position (Fun a _) = a
position (Operator a _) = a
position (Variable a _) = a
position (Application a _ _) = a
position (Function a _ _ _ _ _) = a
position (Datatype a _) = a
position Wildcard = undefined
