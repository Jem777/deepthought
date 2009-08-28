module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

data Datatype = Atom String
            | List [Argument]
            | Tupel [Argument]
            | Number Integer
            | Double Double
            | String String
            | Char Char
            deriving (Show)

data Argument = Type Datatype
            | Variable [Char]
            deriving (Show)

data Lambda = Lambda [Argument] Datatype

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

getArgs (Lambda x _) = x

parseDatatype :: CharParser st Datatype
parseDatatype =
        parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseDouble
    <|> parseChar
    <|> parseTupel
    <|> parseList
    <?> "a datatype"

parseAtom :: CharParser st Datatype
parseAtom = atomId >>= return . Atom

parseString :: CharParser st Datatype
parseString = stringLiteral >>= return . String 

parseNumber :: CharParser st Datatype
parseNumber = integer >>= return . Number

parseDouble :: CharParser st Datatype
parseDouble = float >>= return . Double

parseChar :: CharParser st Datatype
parseChar = charLiteral >>= return . Char

parseList :: CharParser st Datatype
parseList = squares (commaSep value) >>= return . List

parseTupel :: CharParser st Datatype
parseTupel = parens (commaSep value) >>= return . Tupel

parseVar :: CharParser st Argument
parseVar = varId >>= return . Variable

parseType :: CharParser st Argument
parseType = parseDatatype >>= return . Type

parseLambda = parseType

value :: CharParser st Argument
value = parseVar <|> parseType
