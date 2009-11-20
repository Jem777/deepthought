module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

data Datatype = List [Expression]
            | ListComp Pattern [Expression]
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            deriving (Show)

data Pattern = Var [Char]
            | ListConst Pattern Pattern
            | Type Datatype
            | Wildcard
            deriving (Show)

data Expression = Variable [Char]
            | Application Declaration [Expression]
            | Datatype Datatype
            deriving (Show)

data Declaration = Function String [Pattern] Expression
            | Name String
            | Lambda [Pattern] Expression
            deriving (Show)

parseDatatype :: CharParser st Datatype
parseDatatype =
        number
    <|> double
    <|> charTok
    <|> stringTok
{-    <|> tupel
    <|> list-}
    <?> "a datatype"


stringTok :: CharParser st Datatype
stringTok = stringLiteral >>= return . String 

number :: CharParser st Datatype
number = integer >>= return . Number

double :: CharParser st Datatype
double = float >>= return . Float

charTok :: CharParser st Datatype
charTok = charLiteral >>= return . Char

--list :: CharParser st Datatype
--list = squares (commaSep parseStatement) >>= return . List

--tupel :: CharParser st Datatype
--tupel = parens (commaSep parseStatement) >>= return . Tupel

