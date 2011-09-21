module ASTErrors where

import Text.ParserCombinators.Parsec.Pos (SourcePos)
import CompilerErrors

data RuntimeError =
    TooMuchArgs SourcePos String Integer Integer
    | TypeError SourcePos String String
    | PatternException SourcePos String String
    | GuardMismatch SourcePos String
    | BlubbError
    | CompilerError CompileError
    deriving (Show)
    -- | TypeException SourcePos String [ASTDatatype]

instance Eq RuntimeError where
    (TooMuchArgs _ _ _ _) == (TooMuchArgs _ _ _ _) = True
    (TypeError _ _ _) == (TypeError _ _ _) = True
    (PatternException _ _ _) == (PatternException _ _ _) = True
    _ == _ = False

functionError pos opName expected args
        | (toInteger (length args)) > expected = TooMuchArgs pos opName expected (toInteger (length args))
        | otherwise = TypeError pos opName (show args)
