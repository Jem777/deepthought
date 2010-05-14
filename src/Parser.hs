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

parser = parseFromFile deepthought

testParse = f . (parse deepthought "")
        where
            f (Right x) = show x
            f (Left x) = show x

deepthought :: GenParser Char st Tree
deepthought = f <$> (whiteSpace *> parseModule) <*> many parseCompile <*> many parseExport <*> many parseImport <*> ((many function) <* eof)
        where f a b c = Tree a (concat b) (concat c)

parseModule = (reserved "module") >> moduleId

parseImport = do --TODO: restructure this - it probably needs a new type
    (reserved "import")
    mod <- modSep moduleId
    modname <- (option (concatWith mod moduleOp) (reserved "as" >> moduleId))
    return (mod, modname)

parseExport :: GenParser Char st [String]
parseExport = reserved "export" >> squares (commaSep funcId)

parseCompile :: GenParser Char st [String]
parseCompile = reserved "compile" >> squares (commaSep funcId)

function = f <$> (funHead <|> opHead) <*> guard <*> body <*> (funTail <|> closure)
        where f a = Function (fst a) (snd a)

helperFunc = f <$> (funHead <|> opHead) <*> guard <*> body <*> funTail
        where f a = Function (fst a) (snd a)

funHead = (,) <$> fun <*> many (try pattern)

opHead :: GenParser Char st (Datatype, [Expression])
opHead = f <$> pattern <*> op <*> many1 (try pattern)
        where f arg ident args = (ident, arg:args)

body = reservedOp "->" >> expression 

funTail :: GenParser Char st [Expression]
funTail = semi >> return []

closure = reserved "where" >> (parens (many1 helperFunc)) <|> (helperFunc >>= return . (:[]))
guard = option (Datatype (Atom "true")) (reserved "when" >> expression)
