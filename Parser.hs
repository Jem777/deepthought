module Parser (parseAll)
    where

--
-- a parser for my new language
--

import ApplicativeParsec
import Lexer 
import Types 
import Expr

parseAll = parse everything ""

everything = do 
    whiteSpace 
    modulename <- parseModule
    exports <- (many1 parseExport)
    imports <- (many parseImport)
    funcs <- (many function)
    return (Tree modulename (concat exports) imports funcs)

parseModule = (reserved "module") >> moduleId

parseImport = do --TODO: restructure this - it probably need a new type
    (reserved "import")
    mod <- modSep moduleId
    modname <- (option (concatWith mod moduleOp) (reserved "as" >> moduleId))
    return (mod, modname)

parseExport :: GenParser Char st [String]
parseExport = reserved "export" >> squares (commaSep funcId)

function = f <$> (funHead <|> opHead) <*> (reservedOp "->" *> expression)
        where f a b = Function (fst a) (snd a) b

funHead = (,) <$> funcId <*> many pattern
opHead = do
        arg <- pattern
        ident <- operator
        args <- many1 pattern
        return (ident, (arg:args))
