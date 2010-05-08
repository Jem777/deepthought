module Parser 
    where

--
-- a parser for my new language
--

-- Parsing TODOs:
-- list comprehention
-- maybe better import

import ApplicativeParsec
import Lexer 
import Types 
import Expr

parseAll = parse deepthought ""

testParse y z = f (parse y "" z)
        where
            f (Right x) = show x
            f (Left x) = show x

deepthought :: GenParser Char st Tree
deepthought = f <$> (whiteSpace *> parseModule) <*> many1 parseExport <*> many parseImport <*> many function
        where f a b = Tree a (concat b)

parseModule = (reserved "module") >> moduleId

parseImport = do --TODO: restructure this - it probably need a new type
    (reserved "import")
    mod <- modSep moduleId
    modname <- (option (concatWith mod moduleOp) (reserved "as" >> moduleId))
    return (mod, modname)

parseExport :: GenParser Char st [String]
parseExport = reserved "export" >> squares (commaSep funcId)

function = f <$> (funHead <|> opHead) <*> guard <*> body <*> closure
        where f a = Function (fst a) (snd a)

funHead = (,) <$> atom <*> many (try pattern)

opHead :: GenParser Char st (Datatype, [Expression])
opHead = f <$> pattern <*> op <*> many1 (try pattern)
        where f arg ident args = (ident, arg:args)

body = reservedOp "->" >> expression 
closure = option [] (reserved "where" >> parens (many1 function))
guard = option (Datatype (Atom "true")) (reserved "when" >> expression)
