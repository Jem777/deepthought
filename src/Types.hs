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

class Object a where
    position :: a -> SourcePos
    name :: a -> String

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
    Variable SourcePos String
    | Operator SourcePos String String
    | Application SourcePos Expression [Expression]
    -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
    | Function SourcePos Expression [Expression] Expression Expression [Expression]
    -- !!!deprecated!!!
    --first is the ident, second the args, third the guard, fourth the body, fifth closures
    | Datatype SourcePos Datatype
    | Wildcard
    deriving (Show)

data Definition =
    Definition SourcePos String [([Expression], Expression, Expression, [InlineFunction])]
    -- Arguments: Position, FunctionName, [(Variables, Guard, Body, InlineFunctions)]
    deriving (Show)
data InlineFunction =
    InlineFunction SourcePos [([Expression], Expression, Expression)]
    -- Arguments: Position, [(Variables, Guard, Body)]
    deriving (Show)


data TreeObject = Expression (Integer, Integer) String Expression

data Tree = Tree String [String] [Expression] [([String], String)] [Expression] -- modname, compileflags, exports, imports, functions
    deriving (Show, Eq)

data State =
    State [(String, Tree)] [(String, Definition)] [(String, Datatype)]
    -- totally deprecated!
    -- arguments are imported modules, functions and variables

type SourcePos = P.SourcePos

-- instances for the types

instance Monad (Either a) where
    return = Right
    x >>= f = either Left f x

instance Show Datatype where
    show (Number a) = show a
    show (Float a) = show a
    show (String a) = show a
    show (Char a) = show a
    show (Atom a) = a

instance Eq Expression where
    (Variable _ a) == (Variable _ b) = a == b
    (Operator _ a b) == (Operator _ a' b') = a == a' && b == b'
    (Application _ a b) == (Application _ a' b') = a == a' && b == b'
    (Function _ a b c d e) == (Function _ a' b' c' d' e') = a == a' && b == b' && c == c' && d == d' && e == e'
    (Datatype _ a) == (Datatype _ b) = a == b
    Wildcard == Wildcard = True
    _ == _ = False

instance Object Expression where
    position (Variable a _) = a
    position (Operator a _ _) = a
    position (Application a _ _) = a
    position (Function a _ _ _ _ _) = a
    position (Datatype a _) = a
    name (Variable _ a) = a
    name (Operator _ "" a) = a
    name (Operator _ a b) = a ++ "::" ++ b
    name (Application _ a _) = name a
    name (Function _ a _ _ _ _) = name a
    name (Datatype _ _) = "datatype"

instance Object Definition where
    position (Definition a _ _) = a
    name (Definition _ a _) = a

-- a lot of trivial functions for using the types

testEmptyPos = P.newPos "" 0 0
testEmptyState = State [] [("simple", testSimpleFun)] []
testSimpleFun = Definition testEmptyPos "simple"
    [(
        [Variable testEmptyPos "X"],
        Datatype testEmptyPos (Atom "@true"),
        (Application testEmptyPos (Operator testEmptyPos "" "+") [Datatype testEmptyPos (Number 2),Variable testEmptyPos "X"]),
        []
    )]

-- functions for the state

getVariable (State _ _ a) b = lookup b a
getFunction (State _ a _) b = lookup b a
getTree (State a _ _) b = lookup b a
setVariable (State a b _) c = State a b c
setFunction (State a _ c) b = State a b c
addVariables (State a b c) d = State a b (union c d)
addFunctions (State a b c) d = State a (d ++ b) c
addFunction (State a b c) d = State a (d:b) c
setTree (State _ b c) a = State a b c

-- functions for datatypes

getType :: Datatype -> String
getType (List _) = "List"
getType (Vector _) = "Vector"
getType (Number _) = "Integer"
getType (Float _) = "Float"
getType (String _) = "String"
getType (Char _) = "Char"
getType (Atom _) = "Atom"

-- functions for the tree

treeName (Tree a _ _ _ _) = a
treeCompile (Tree _ a _ _ _) = a
treeExports (Tree _ _ a _ _) = a
treeImports (Tree _ _ _ a _) = a
treeFuncs (Tree _ _ _ _ a) = a

lambdaArgs (Lambda a _) = a
lambdaBody (Lambda _ a) = a

appName (Application _ a _) = a
appArgs (Application _ _ a) = a
funcName (Function _ a _ _ _ _) = a
funcArgs (Function _ _ a _ _ _) = a
funcGuard (Function _ _ _ a _ _) = a
funcBody (Function _ _ _ _ a _) = a
funcWhere (Function _ _ _ _ _ a) = a
dataType (Datatype _ a) = a

isList (List _) = True
isList _ = False
isVector (Vector _) = True
isVector _ = False
isVar (Variable _ _) = True
isVar _ = False
isOp (Operator _ _ _) = True
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
