module Parser (parseAll)
    where

--
-- a parser for my new language
--
import Text.ParserCombinators.Parsec
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
    imports <- (many parseImport)
    exports <- (many1 parseExport)
    return (modulename, imports, concat exports)

parseModule = do 
    (reserved "module")
    moduleId

parseImport = do 
    (reserved "import")
    mod <- modSep moduleId
    modname <- (option (concatWith mod moduleOp) (reserved "as" >> moduleId))
    return (mod, modname)

parseExport = do 
    (reserved "export")
    squares (commaSep funcId)

declaration = do
    left <- function
    reservedOp "->"
    right <- many1 expression
    return (left, right)

function = 
    try ( do 
        ident <- funcId
        args <- many pattern
        return (ident, args))
    <|> ( do
        arg <- pattern
        ident <- operator
        args <- many1 pattern
        return (ident, (arg:args)))

