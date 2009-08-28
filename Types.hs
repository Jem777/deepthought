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
            | Bool Bool
            deriving (Show)

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

parseAtom :: CharParser st Datatype
parseAtom = lowerId >>= \e -> return (Atom e)

parseString :: CharParser st Datatype
parseString = stringLiteral >>= \e -> return (String e)

