module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

data Datatype = Atom String
            | List [Datatype]
            | Number Integer
            | Double Double
            | String String
            | Char Char
            deriving (Show)

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

parseType :: CharParser st Datatype
parseType =
        parseAtom
    <|> parseString
    <|> parseList
    <?> "a datatype"

parseOneType f =
        (f parseAtom)
    <|> (f parseString)
    <|> (f parseList)

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
parseList = squares (parseOneType commaSep) >>= return . List
