module CompilerErrors where

import Text.ParserCombinators.Parsec.Error

data CompileError =
    FoobarError
    | ParseError ParseError
    deriving (Show)
