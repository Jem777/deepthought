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
            | Function String [Expression] Expression --first is the ident, second the args, third the body
            | Datatype Datatype
            | Wildcard
            deriving (Show)

data Tree = Tree String [String] [([String], String)] [Expression] -- modname, exports, imports, functions
            deriving (Show)
-- some really trivial functions

atom :: CharParser st Datatype
atom = funcId >>= return . Atom

bool :: CharParser st Datatype
bool = boolId >>= return . Atom

stringTok :: CharParser st Datatype
stringTok = stringLiteral >>= return . String 

number :: CharParser st Datatype
number = integer >>= return . Number

double :: CharParser st Datatype
double = float >>= return . Float

charTok :: CharParser st Datatype
charTok = charLiteral >>= return . Char

list x = squares (commaSep x) >>= return . List

tupel x = parens (commaSep x) >>= return . Tupel
