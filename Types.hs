module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

data Datatype = List [Expression]
            | ListComp Expression [Expression] -- first one is a pattern
            | Tupel [Expression]
            | Number Integer
            | Float Double
            | String String
            | Char Char
            | Function String
            | Lambda [Expression] Expression --first one is a pattern
            deriving (Show)

data Expression = Variable [Char]
            | Application Expression [Expression]
            | Datatype Datatype
            | Wildcard
            deriving (Show)

datatype :: CharParser st Datatype
datatype =
        number
    <|> double
    <|> charTok
    <|> stringTok
    <?> "a datatype"

pattern :: CharParser st Expression
pattern = 
        parens (listconstructor)
    <|> (datatype >>= return . Datatype) 
    <|> (varId >>= return . Variable)
    <|> (wildcard >> return Wildcard)
    <|> (list pattern >>= return . Datatype)
    <|> (tupel pattern >>= return . Datatype)
    <?> "a pattern"

listconstructor = do
    first <- pattern
    reservedOp ":"
    last <- pattern
    return (Application (Datatype (Function ":")) [first, last])

expression = 
    try application
    <|> try infixOp
    <|> expr

expr = 
        parens (expression)
    <|> (datatype >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "an expression"

application = do
    fun <- expr <|> func
    arg <- (many1 expr)
    return (Application fun arg)

infixOp = do
    first <- expr
    op <- operator
    last <- (many1 expr)
    return (Application (Datatype (Function op)) (first : last))

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

