module Lexer 
    where
-- 
-- the lexer for my new language
--

import ApplicativeParsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Types

languageDef
    = P.LanguageDef
    { P.commentStart   = "{-"
    , P.commentEnd     = "-}"
    , P.commentLine    = "--"
    , P.nestedComments = True
    , P.identStart     = lower
    , P.identLetter    = alphaNum <|> char '_'
    , P.opStart        = oneOf ":!#$%&*+./<=>?\\^|-~"
    , P.opLetter       = oneOf ":!#$%&*+./<=>?\\^|-~"
    , P.reservedOpNames= ["::","..","=","\\","|","<-","->","~"]
    , P.reservedNames  = ["let","in","case","of"
                       ,"if","then","else"
                       ,"data","type"
                       ,"class","default","deriving"
                       ,"infix","infixl","infixr"
                       ,"instance","do"
                       ,"newtype"
                       ,"primitive", 
                       "module","import", "export", "compile",
                       "where", "when", "private", "public"
                       ]          
    , P.caseSensitive  = True                                   
    }

lexer = P.makeTokenParser languageDef

whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
semiSep     = P.semiSep lexer
identifier  = P.identifier lexer
operator    = P.operator lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
braces      = P.braces lexer
squares     = P.squares lexer
colon       = P.colon lexer
commaSep    = P.commaSep lexer
commaSep1   = P.commaSep1 lexer
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
integer     = P.integer lexer
float       = P.float lexer


lowerId = P.identifier lexer
upperId :: GenParser Char st [Char]
upperId = (:) <$> upper <*> ((many $ alphaNum <|> char '_') <* whiteSpace)
upperId2 = (:) <$> upper <*> (many $ alphaNum <|> char '_')

boolId = 
    ((reserved "true") >> return "true") 
    <|> ((reserved "false") >> return "false")

wildcard = symbol "_" <?> "wildcard"
funcId = lowerId <?> "function"
atomId = (:) <$> char '@' <*> (lowerId <?> "atom")
moduleId = upperId <?> "module"
varId = upperId <?> "variable"
moduleOp = "::"
modSep x    = sepBy1 x (reservedOp moduleOp) 
colonSep p  = sepBy p colon
infixFun = between (char '`') (char '`') funcId <?> "function"
prefixOperator = parens operator <?> "operator"

concatWith [] sep  =  []
concatWith ws sep  =  foldr1 (\w s -> w ++ sep ++ s) ws

