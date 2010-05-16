module Types
    where

import Text.ParserCombinators.Parsec.Pos

--
-- definition of all necessary types for parsing
-- and types for an intermediate byte-code

data Datatype = --primitve datatypes and lists and tupels
            List [Expression]
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Atom String
            deriving (Show, Eq)

data Expression = -- everything that evals to an datatype
            Variable SourcePos String
            | Fun SourcePos String
            | Operator SourcePos String
            | Application SourcePos Expression [Expression]
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Lambda SourcePos [Expression] Expression --[Expr] are the arguments, Expr is the Body
            | Function SourcePos Expression [Expression] Expression Expression [Expression] 
            --first is the ident, second the args, third the guard, fourth the body, fifth closures
            | Datatype SourcePos Datatype
            | Wildcard
            deriving (Show, Eq)

data TreeObject = Expression (Integer, Integer) String Expression

data Tree = Tree String [String] [String] [([String], String)] [Expression] -- modname, compileflags, exports, imports, functions
            deriving (Show, Eq)

data CompileError = CompileError String SourcePos String --kind of Error, SourcePos, Message

instance Show CompileError where
    show (CompileError a b c) = a ++ " at " ++ show b ++ ":\n" ++ c


treeName (Tree a _ _ _ _) = a
treeCompile (Tree _ a _ _ _) = a
treeExports (Tree _ _ a _ _) = a
treeImports (Tree _ _ _ a _) = a
treeFuncs (Tree _ _ _ _ a) = a

varName (Variable _ a) = a
appName (Application _ a _) = a
appArgs (Application _ _ a) = a
lambdaArgs (Lambda _ a _) = a
lambdaBody (Lambda _ _ a) = a
funcName (Function _ a _ _ _ _) = a
funcArgs (Function _ _ a _ _ _) = a
funcGuard (Function _ _ _ a _ _) = a
funcBody (Function _ _ _ _ a _) = a
funcWhere (Function _ _ _ _ _ a) = a
opName (Operator _ a) = a
funName (Fun _ a) = a
--datatype (Datatype a) = a

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
isLambda (Lambda _ _ _) = True
isLambda _ = False
isFunction (Function _ _ _ _ _ _) = True
isFunction _ = False
isDatatype (Datatype _ _) = True
isDatatype _ = False
isWildcard Wildcard = True
isWildcard _ = False

value (Operator _ a) = a
value (Fun _ a) = a

position (Fun a _) = a
position (Operator a _) = a
position (Variable a _) = a
position (Application a _ _) = a
position (Lambda a _ _) = a
position (Function a _ _ _ _ _) = a
position (Datatype a _) = a
position Wildcard = undefined
