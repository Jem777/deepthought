module Types
    where

--
-- definition of all necessary types for parsing
-- and types for an intermediate byte-code

import Lexer
import Text.ParserCombinators.Parsec

data Datatype = --primitve datatypes and lists and tupels
            List [Expression]
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Atom String
            | Operator String
            deriving (Show)

data Expression = -- everything that evals to an datatype
            Variable [Char]
            | Application Expression [Expression]
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Lambda [Expression] Expression --[Expr] are the arguments, Expr is the Body
            | Function Datatype [Expression] Expression Expression [Expression] --first is the ident, second the args, third the guard, fourth the body, fifth closures
            | Datatype Datatype
            | Wildcard
            deriving (Show)

data Tree = Tree String [String] [([String], String)] [Expression] -- modname, exports, imports, functions
            deriving (Show)


-- some really trivial functions

fun = funcId >>= return . Atom
atom = fun
bool = boolId >>= return . Atom
str = stringLiteral >>= return . String 
number = integer >>= return . Number
double = float >>= return . Float
chr = charLiteral >>= return . Char
op = operator >>= return . Operator

list x = squares (commaSep x) >>= return . List
tupel x = parens (commaSep x) >>= return . Tupel
