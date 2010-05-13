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
        [binary "==" AssocLeft, binary "!=" AssocLeft, binary "<" AssocLeft, binary "<=" AssocLeft, binary ">" AssocLeft, binary ">=" AssocLeft],
        [binary "&&" AssocRight],
        [binary "||" AssocRight],
        [binary ">>=" AssocLeft, binary ">>" AssocLeft]
        ]

binary  name assoc = Infix (do{ reservedOp name; return (\x y -> op_to_expr name [x,y])}) assoc
prefix  name       = Prefix (do{ reservedOp name; return (\y -> prefixop_to_expr name [y]) })
postfix name       = Postfix (do{ reservedOp name; return (\y -> op_to_expr name [y]) })

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
    <|> (primitive >>= return . Datatype) 
    <|> (varId >>= return . Variable)
    <|> (wildcard >> return Wildcard)
    <|> (list pattern >>= return . Datatype)
    <|> (tupel pattern >>= return . Datatype)
    <?> "pattern"

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
expression :: GenParser Char st Expression
expression = 
    try application
    <|> try (buildExpressionParser table expr)
    <|> try (parens lambda) 
    <|> expr

expr = 
    try (parens expression)
    <|> (primitive >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "expression"

appHead :: GenParser Char st Expression
appHead = 
    try (parens lambda) 
    <|> (fun >>= return . Datatype)
    <|> (prefixOp >>= return . Datatype . Operator)
    <|> (varId >>= return . Variable)

application :: GenParser Char st Expression
application = Application <$> appHead <*> (many1 (try (fun >>= return . Datatype) <|> expr))

lambda :: CharParser st Expression
lambda = Lambda <$> ((reservedOp "\\") *> (commaSep1 (varId >>= return . Variable))) <*> ((reservedOp "->") *> expression)

-- some really trivial functions

fun = funcId >>= return . Atom
atom = atomId >>= return . Atom
bool = boolId >>= return . Atom
str = stringLiteral >>= return . String 
number = natural >>= return . Number
double = float >>= return . Float
chr = charLiteral >>= return . Char
op = operator >>= return . Operator

list x = squares (commaSep x) >>= return . List
tupel x = parens (commaSep x) >>= return . Tupel

-- internal functions

op_to_expr :: String -> [Expression] -> Expression
op_to_expr = Application . Datatype . Operator

prefixop_to_expr :: String -> [Expression] -> Expression
prefixop_to_expr = Application . Datatype . convert
    where
    convert "+" = Atom "id"
    convert "-" = Atom "neg"
    convert x = Operator x

