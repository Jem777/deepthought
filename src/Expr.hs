module Expr
    where

--
-- datatypes and some functions to parse and output them
--

import Lexer
import Types
import ApplicativeParsec
import Text.ParserCombinators.Parsec.Expr


table :: OperatorTable Char st Expression
table   = [ 
        [prefix "+", prefix "-"],
        [binary "." AssocRight],
        [binary "**" AssocRight, binary "^" AssocRight, binary "^^" AssocRight],
        [postfix "++"], 
        [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft], 
        [binary "+" AssocLeft, binary "-" AssocLeft],
        [binary ":" AssocRight],
        [binary "==" AssocLeft, binary "!=" AssocLeft, binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft],
        [binary "&&" AssocRight],
        [binary "||" AssocRight],
        [binary ">>=" AssocLeft, binary ">>" AssocLeft]
        ]

binary  name = Infix (posOp name >>= (\p -> return (\x y -> op_to_expr p name [x,y])))
prefix  name = Prefix (posOp name >>= (\p -> return (\y -> prefixop_to_expr p name [y])))
postfix name = Postfix (posOp name >>= (\p -> return (\y -> op_to_expr p name [y])))
--binFunc name = Infix (reserved name >> return (\x y -> (Application . Datatype . Atom) name [x,y]))

posOp name = getPosition <* (reservedOp name)

primitive :: CharParser st Datatype
primitive =
        (try double)
    <|> number
    <|> chr
    <|> str
    <|> atom
    <|> fun
    <?> "primitive" 

pattern :: CharParser st Expression
pattern = 
        (parens listconstructor)
    <|> datatype primitive 
    <|> var
    <|> (wildcard >> return Wildcard)
    <|> datatype (list pattern)
    <|> datatype (tupel pattern)
    <?> "pattern"

listconstructor = f <$> getPosition <*> colonSep pattern
        where f x = Application x (Datatype x (Operator ":"))
{-
listconstructor = 
    colonSep pattern >>= 
    return . (Application (Datatype (Operator ":")))
-}
{-listcomprehension = 
    squares (
            do 
            patr <- pattern
            reservedOp "|"
            compr <- (commaSep1 expression)
            return (ListComp patr compr)
            )
-}
expression :: GenParser Char st Expression
expression =
    try application
    <|> try (buildExpressionParser table expr)
    <|> lambda
    <|> expr

expr =
    try (parens expression)
    <|> datatype primitive
    <|> datatype (list expression)--(list expression >>= return . Datatype)
    <|> datatype (tupel expression)--(tupel expression >>= return . Datatype)
    <|> var
    <?> "expression"

appHead :: GenParser Char st Expression
appHead = 
    (parens expression)
    <|> datatype fun
    <|> datatype prefixOp
    <|> var

application :: GenParser Char st Expression
application = Application <$> getPosition <*> appHead <*> (many1 ((datatype fun) <|> expr))

lambda :: CharParser st Expression
lambda = Lambda <$> ((reservedOp "\\") *> getPosition) <*> (commaSep1 var) <*> ((reservedOp "->") *> expression)

-- some really trivial functions

fun = Fun <$> funcId 
atom = Atom <$> atomId 
bool = Atom <$> boolId 
str = String <$> stringLiteral 
number = Number <$> natural
double = Float <$> float
chr = Char <$> charLiteral
op = Operator <$> operator
prefixOp = Operator <$> prefixOperator

list x = List <$> squares (commaSep x)
tupel x = Tupel <$> parens (commaSep x)

var = Variable <$> getPosition <*> varId
datatype x = Datatype <$> getPosition <*> x

-- fmap rules

-- internal functions

op_to_expr :: SourcePos -> String -> [Expression] -> Expression
op_to_expr pos name = Application pos (Datatype pos (Operator name))

prefixop_to_expr :: SourcePos -> String -> [Expression] -> Expression
prefixop_to_expr pos name = Application pos (Datatype pos (convert name))
    where
    convert "+" = Atom "id"
    convert "-" = Atom "neg"
    convert x = Operator x

