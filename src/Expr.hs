module Expr
    where

--
-- datatypes and expressions 
-- and some functions to parse and output them
--

import Lexer
import Types
import ApplicativeParsec hiding (SourcePos)
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
binFunc name = Infix (f >>= (\p -> return (\x y -> (Application p (Operator p "" name) [x,y]))))
        where f = getPosition <* (reservedOp name)

posOp name = getPosition <* (reservedOp name)

primitive :: CharParser st Datatype
primitive =
        (try double)
    <|> number
    <|> chr
    <|> str
    <|> atom
    <?> "primitive" 

pattern :: CharParser st Expression
pattern = 
        (parens listconstructor)
    <|> datatype primitive 
    <|> var
    <|> (wildcard >> return Wildcard)
    <|> datatype (list pattern)
    <|> datatype (vector pattern)
    <?> "pattern"

listconstructor = f <$> getPosition <*> colonSep pattern
        where f x = Application x (Operator x "" ":")

{-listcomprehension = 
    squares (
            do 
            patr <- pattern
            reservedOp "|"
            vars <- (commaSep1 (var <* reservedOp "<-" list))
            compr <- (commaSep expression)
            return (ListComp patr compr)
            )
-}

expression :: GenParser Char st Expression
expression =
    try application
    <|> try (buildExpressionParser table expr)
    <|> datatype lambda
    <|> expr

expr =
    try (parens expression)
    <|> try fun
    <|> datatype primitive
    <|> datatype (list expression)
    <|> datatype (vector expression)
    <|> var
    <|> prefixOp
    <?> "expression"

appHead :: GenParser Char st Expression
appHead = 
    try (parens expression)
    <|> var
    <|> fun
    <|> prefixOp

application :: GenParser Char st Expression
application = Application <$> getPosition <*> appHead <*> (many1 expr) <?> "function"

lambda :: CharParser st Datatype
lambda = Lambda <$> ((reservedOp "\\") *> (commaSep1 var)) <*> ((reservedOp "->") *> expression)

-- some really trivial functions

-- datatypes
atom = Atom <$> atomId 
bool = Atom <$> boolId 
str = String <$> stringLiteral 
number = Number <$> natural
double = Float <$> float
chr = Char <$> charLiteral

list x = List <$> squares (commaSep x)
vector x = Vector <$> parens (commaSep x)

-- expressions
bareFun = Operator <$> getPosition <*> return "" <*> (funcId <|> parens operator)
bareOp = Operator <$> getPosition <*> return "" <*> operator
qualifiedFun = Operator <$> getPosition <*> (upperId2 <* (string moduleOp)) <*> (funcId <|> parens operator)
qualifiedOp = Operator <$> getPosition <*> (upperId2 <* (string moduleOp)) <*> operator
fun = bareFun <|> qualifiedFun
op = bareOp <|> qualifiedOp
prefixOp = barePrefixOp <|> qualifiedPrefixOp
barePrefixOp = Operator <$> getPosition <*> return "" <*> prefixOperator
qualifiedPrefixOp = Operator <$> getPosition <*> (upperId2 <* (string moduleOp)) <*> prefixOperator
var = Variable <$> getPosition <*> varId
datatype x = Datatype <$> getPosition <*> x

-- fmap rules

-- internal functions

op_to_expr :: SourcePos -> String -> [Expression] -> Expression
op_to_expr pos name = Application pos (Operator pos "" name)

prefixop_to_expr :: SourcePos -> String -> [Expression] -> Expression
prefixop_to_expr pos name = Application pos (convert name)
    where
    convert "+" = Operator pos "" "id"
    convert "-" = Operator pos "" "neg"
    convert x = Operator pos "" x

