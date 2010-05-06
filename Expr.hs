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

primitive :: CharParser st Datatype
primitive =
        number
    <|> double
    <|> charTok
    <|> stringTok
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
expression = 
    try application
    <|> try (buildExpressionParser table expr)
    <|> try (parens lambda >>= return . Datatype)
    <|> expr

expr = 
    try (parens expression)
    <|> (primitive >>= return . Datatype) 
    <|> (list expression >>= return . Datatype)
    <|> (tupel expression >>= return . Datatype)
    <|> (varId >>= return . Variable)
    <?> "expression"

appHead = 
    (parens lambda >>= return . Datatype)
    <|> (atom >>= return . Datatype)
    <|> (varId >>= return . Variable)

application :: GenParser Char st Expression
application = Application <$> appHead <*> (many1 expr)

lambda :: CharParser st Datatype
lambda = Lambda <$> ((reservedOp "\\") *> (commaSep1 pattern)) <*> ((reservedOp "->") *> expression)

-- internal functions

op_to_expr :: String -> [Expression] -> Expression
op_to_expr = Application . Datatype . op_to_atom

op_to_atom :: String -> Datatype
op_to_atom = Atom . convert
    where
    convert "+" = "add"
    convert x = x

