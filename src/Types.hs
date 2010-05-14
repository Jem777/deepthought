module Types
    where

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
            | Operator String
            deriving (Show, Eq)

data Expression = -- everything that evals to an datatype
            Variable [Char]
            | Application Expression [Expression]
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Lambda [Expression] Expression --[Expr] are the arguments, Expr is the Body
            | Function Datatype [Expression] Expression Expression [Expression] --first is the ident, second the args, third the guard, fourth the body, fifth closures
            | Datatype Datatype
            | Wildcard
            deriving (Show, Eq)

data Tree = Tree String [String] [String] [([String], String)] [Expression] -- modname, compileflags, exports, imports, functions
            deriving (Show, Eq)


treeName (Tree a _ _ _ _) = a
treeCompile (Tree _ a _ _ _) = a
treeExports (Tree _ _ a _ _) = a
treeImports (Tree _ _ _ a _) = a
treeFuncs (Tree _ _ _ _ a) = a

varName (Variable a) = a
appName (Application a _) = a
appArgs (Application _ a) = a
lambdaArgs (Lambda a _) = a
lambdaBody (Lambda _ a) = a
funcName (Function a _ _ _ _) = a
funcArgs (Function _ a _ _ _) = a
funcGuard (Function _ _ a _ _) = a
funcBody (Function _ _ _ a _) = a
funcWhere (Function _ _ _ _ a) = a
datatype (Datatype a) = a

atomName (Atom a) = a
opName (Operator a) = a
