module Parser (parseAll)
    where

--
-- a parser for my new language
--
import Text.ParserCombinators.Parsec
import Lexer 
import Types 

parseAll = parse everything

everything = do
    header <- parseHeader
    funs <- many parseFun
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

parseFun :: GenParser Char st ([Char], [Argument], [Char])
parseFun = do
    ident <- funcId
    args <- parens (commaSep value)
    reservedOp "->"
    body <- parseStatement
    return (ident, args, body)

parseStatement = varId <|> funcId

