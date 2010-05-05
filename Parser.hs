module Parser (parseAll)
    where

--
-- a parser for my new language
--
--import Text.ParserCombinators.Parsec
import ApplicativeParsec
import Lexer 
import Types 
import Expr

parseAll = parse everything ""

everything = do
    header <- parseHeader
    funs <- many declaration
    return (header, funs)

parseHeader = do 
    whiteSpace 
    modulename <- parseModule
    exports <- (many1 parseExport)
    imports <- (many parseImport)
    return (modulename, concat exports, imports)

parseModule = do 
    (reserved "module")
    moduleId

parseImport = do --TODO: restructure this - it probably need a new type
    (reserved "import")
    mod <- modSep moduleId
    modname <- (option (concatWith mod moduleOp) (reserved "as" >> moduleId))
    return (mod, modname)

parseExport :: GenParser Char st [String]
parseExport = reserved "export" >> squares (commaSep funcId)
{-parseExport = do 
    (reserved "export")
    squares (commaSep funcId)
-}

declaration = do
    left <- function
    reservedOp "->"
    right <- many1 expression
    return (left, right)

{-function = 
    ( do 
        ident <- funcId
        args <- many pattern
        return (ident, args))
    <|> ( do
        arg <- pattern
        ident <- operator
        args <- many1 pattern
        return (ident, (arg:args)))
-}

function = f <$> (funHead <|> opHead) <*> (reservedOp "->" *> expression)
        where f a b = Function (fst a) (snd a) b

funHead = (,) <$> funcId <*> many pattern
opHead = do
        arg <- pattern
        ident <- operator
        args <- many1 pattern
        return (ident, (arg:args))
