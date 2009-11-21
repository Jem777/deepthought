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
            | Function String
            | Lambda [Pattern] Expression
            deriving (Show)

data Pattern = Var [Char]
            | ListConst Pattern Pattern
            | Type Datatype
            | Wildcard
            deriving (Show)

data Expression = Variable [Char]
            | Application Expression [Expression]
            | Datatype Datatype
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

pattern :: CharParser st Pattern
pattern = 
        (parseDatatype >>= return . Type) 
    <|> (varId >>= return . Var)
    <|> (wildcard >> return Wildcard)

expression = 
    try application
    <|> expr

expr = 
        parens (expression)
    <|> (parseDatatype >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "an expression"

application = do
    fun <- expr <|> func
    arg <- (many1 expr)
    return (Application fun arg)

lambda = do 
    reservedOp "\\"
    vars <- (many1 (try pattern))
    reservedOp "->"
    expression <- expression
    return (Lambda vars expression)

func :: CharParser st Expression
func = funcId >>= return . Datatype . Function

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

