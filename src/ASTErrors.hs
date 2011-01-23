module ASTErrors where

import Text.ParserCombinators.Parsec.Pos (SourcePos)

data RuntimeError =
    TooMuchArgs SourcePos String Integer Integer
    | TypeError SourcePos String String
    | PatternException SourcePos
    | BlubbError
    deriving (Show)
    -- | TypeException SourcePos String [ASTDatatype]

instance Eq RuntimeError where
    (TooMuchArgs _ _ _ _) == (TooMuchArgs _ _ _ _) = True
    (TypeError _ _ _) == (TypeError _ _ _) = True
    (PatternException _) == (PatternException _) = True
    _ == _ = False

functionError pos opName expected args
        | (toInteger (length args)) > expected = TooMuchArgs pos opName expected (toInteger (length args))
        | otherwise = TypeError pos opName (show args)
