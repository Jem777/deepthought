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
binFunc name = Infix (f >>= (\p -> return (\x y -> (Application p (Fun p name) [x,y]))))
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
    <|> datatype (tupel pattern)
    <?> "pattern"

listconstructor = f <$> getPosition <*> colonSep pattern
        where f x = Application x (Operator x ":")

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
    <|> lambda
    <|> expr

expr =
    try (parens expression)
    <|> datatype primitive
    <|> datatype (list expression)
    <|> datatype (tupel expression)
    <|> var
    <|> fun
    <|> prefixOp
    <?> "expression"

appHead :: GenParser Char st Expression
appHead = 
    (parens expression)
    <|> var
    <|> fun
    <|> prefixOp

application :: GenParser Char st Expression
application = Application <$> getPosition <*> appHead <*> (many1 (fun <|> expr))

lambda :: CharParser st Expression
lambda = Lambda <$> ((reservedOp "\\") *> getPosition) <*> (commaSep1 var) <*> ((reservedOp "->") *> expression)

-- some really trivial functions

-- datatypes
atom = Atom <$> atomId 
bool = Atom <$> boolId 
str = String <$> stringLiteral 
number = Number <$> natural
double = Float <$> float
chr = Char <$> charLiteral

list x = List <$> squares (commaSep x)
tupel x = Tupel <$> parens (commaSep x)

-- expressions
bareFun = Fun <$> getPosition <*> funcId 
bareOp = Operator <$> getPosition <*> operator
qualifiedFun = Fun <$> getPosition <*> ((endBy1 upperId2 (string moduleOp)) *> funcId)
qualifiedOp = Operator <$> getPosition <*> ((endBy1 upperId2 (string moduleOp)) *> operator)
fun = bareFun <|> qualifiedFun
op = bareOp <|> qualifiedOp
prefixOp = Operator <$> getPosition <*> prefixOperator
var = Variable <$> getPosition <*> varId
datatype x = Datatype <$> getPosition <*> x

-- fmap rules

-- internal functions

op_to_expr :: SourcePos -> String -> [Expression] -> Expression
op_to_expr pos name = Application pos (Operator pos name)

prefixop_to_expr :: SourcePos -> String -> [Expression] -> Expression
prefixop_to_expr pos name = Application pos (convert name)
    where
    convert "+" = Fun pos "id"
    convert "-" = Fun pos "neg"
    convert x = Operator pos x

