module Types
    where

--
-- datatypes and some functions to parse and output them
--
import Text.ParserCombinators.Parsec
import Lexer

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

datatype :: CharParser st Datatype
datatype =
        number
    <|> double
    <|> atom
    <|> charTok
    <|> stringTok
    <|> lambda
    <?> "a datatype" 

pattern :: CharParser st Expression
pattern = 
        (parens listconstructor)
    <|> (datatype >>= return . Datatype) 
    <|> (varId >>= return . Variable)
    <|> (wildcard >> return Wildcard)
    <|> (list pattern >>= return . Datatype)
    <|> (tupel pattern >>= return . Datatype)
    <?> "a pattern"

listconstructor = 
    colonSep pattern >>= 
    return . (Application (Datatype (Operator ":")))

{-listcomprehension = 
    squares (
            do 
            patr <- pattern
            reservedOp "|"
            compr <- (commaSep1 expression)
            return (ListComp patr compr)
            )
-}
expression = 
        try infixOp 
    <|> try application
    <|> expr

expr = 
        parens expression
    <|> (datatype >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "an expression"

application = do
    fun <- expr 
    arg <- (many1 expr)
    return (Application fun arg)

infixOp = do
    first <- expr
    op <- operator
    last <- expression
    return (Application (Datatype (Operator op)) [first, last])

lambda = do 
    reservedOp "\\"
    vars <- (many1 (try pattern))
    reservedOp "->"
    expression <- expression
    return (Lambda vars expression)

atom :: CharParser st Datatype
atom = funcId >>= return . Atom

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

