module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

data Datatype = Atom String
            | List [Statement]
            | Tupel [Statement]
            | Number Integer
            | Double Double
            | String String
            | Char Char
            deriving (Show)

data Statement = Bind [Char] Statement Statement
            | Type Datatype
            | Variable [Char]
            | Application [Argument] Statement
            | Function [Char] [Statement]
            | Lambda [Argument] Statement
            deriving (Show)

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

parseDatatype :: CharParser st Datatype
parseDatatype =
        atom
    <|> number
    <|> double
    <|> charTok
    <|> stringTok
    <|> tupel
    <|> list
    <?> "a datatype"

atom :: CharParser st Datatype
atom = atomId >>= return . Atom

stringTok :: CharParser st Datatype
stringTok = stringLiteral >>= return . String 

number :: CharParser st Datatype
number = integer >>= return . Number

double :: CharParser st Datatype
double = float >>= return . Double

charTok :: CharParser st Datatype
charTok = charLiteral >>= return . Char

list :: CharParser st Datatype
list = squares (commaSep value) >>= return . List

tupel :: CharParser st Datatype
tupel = parens (commaSep value) >>= return . Tupel

var :: CharParser st Statement
var = varId >>= return . Variable

datatype :: CharParser st Statement
datatype = parseDatatype >>= return . Type

value :: CharParser st Statement
value = var <|> datatype
