module CompilerErrors where

import Text.ParserCombinators.Parsec.Error
import Misc

data CompileError =
    FoobarError
    | NameError SourcePos String
    | ParseError ParseError
    deriving (Show)
