module Lexer 
    where
-- 
-- the lexer for my new language
--
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

languageDef
    = P.LanguageDef
    { P.commentStart   = "{-"
    , P.commentEnd     = "-}"
    , P.commentLine    = "--"
    , P.nestedComments = True
    , P.identStart     = lower
    , P.identLetter    = alphaNum <|> char '_'
    , P.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"              
    , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"    
    , P.reservedOpNames= ["::","..","=","\\","|"
                       ,"<-","->","@","~","=>"
                       ]
    , P.reservedNames  = ["let","in","case","of"
                       ,"if","then","else"
                       ,"data","type"
                       ,"class","default","deriving"
                       ,"module","import", "export"
                       ,"infix","infixl","infixr"
                       ,"instance","do"
                       ,"newtype","where"
                       ,"primitive"
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
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
braces      = P.braces lexer
squares     = P.squares lexer
commaSep    = P.commaSep lexer
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer
integer     = P.integer lexer
float       = P.float lexer
modSep x    = sepBy1 x (reservedOp moduleOp) 

lowerId = P.identifier lexer
upperId = do
    x <- upper 
    xs <- many (lexeme (alphaNum <|> char '_'))
    return (x:xs)


funcId = lowerId <?> "a function"
moduleId = upperId <?> "a module"
varId = upperId <?> "a variable"
atomId = lowerId <?> "an atom"
moduleOp = "::"

concatWith [] sep  =  []
concatWith ws sep  =  foldr1 (\w s -> w ++ sep ++ s) ws

