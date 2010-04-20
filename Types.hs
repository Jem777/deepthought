module Types
    where

--
-- datatypes and some functions to parse and output them
--

import Lexer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

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


table :: OperatorTable Char st Expression
table   = [ 
        [binary "**" AssocLeft],
        [postfix "++"], 
        [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft], 
        [binary "+" AssocLeft, binary "-" AssocLeft],
        [binary "==" AssocLeft, binary "!=" AssocLeft, binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft],
        [binary "&&" AssocLeft],
        [binary "||" AssocLeft]
        ]

binary  name assoc = Infix (do{ reservedOp name; return (\x y -> op_to_expr name [x,y])}) assoc
prefix  name       = Prefix (do{ reservedOp name; return (\y -> op_to_expr name [y]) })
postfix name       = Postfix (do{ reservedOp name; return (\y -> op_to_expr name [y]) })


datatype :: CharParser st Datatype
datatype =
        number
    <|> double
--    <|> atom
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
    try application
    <|> try (buildExpressionParser table expr)
    <|> (atom >>= return . Datatype)
    <|> expr

expr = 
        parens expression
    <|> (datatype >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "an expression"

application = do
    fun <- (try (atom >>= return . Datatype) <|> expr) 
    arg <- (many1 expr)
    return (Application fun arg)

lambda = do 
    reservedOp "\\"
    vars <- (commaSep1 (try pattern))
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

op_to_expr :: String -> [Expression] -> Expression
op_to_expr = Application . Datatype . op_to_atom

op_to_atom :: String -> Datatype
op_to_atom = Atom . convert
    where
    convert "+" = "add"
    convert x = x
