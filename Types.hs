module Types
    where

--
-- definition of all necessary types for parsing
-- and types for an intermediate byte-code

import Lexer
import Text.ParserCombinators.Parsec

data Datatype = List [Expression]
            -- | ListComp Expression [Datatype] [Expression] -- first one is a pattern
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Atom String
            | Operator String
            | Lambda [Expression] Expression --first one is a pattern
            deriving (Show)

data Expression = Variable [Char]
            | Application Expression [Expression]
            | Datatype Datatype
            | Wildcard
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
