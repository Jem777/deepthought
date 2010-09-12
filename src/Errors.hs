module Errors where

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import Types (position, name, Expression, Datatype)

-- This module provides functions to create errors

data CompileError = CompileError String SourcePos String --kind of Error, SourcePos, Message
data Error = SyntaxError ParseError
    | NameError String SourcePos String
    | VarUnbound Expression
    | Conflicting Expression Expression
    | TooMuchArgs Expression Integer Integer
    | TypeError String SourcePos String
    | PatternException SourcePos

instance Show CompileError where
    show (CompileError a b c) = a ++ " at " ++ show b ++ ":\n" ++ c

instance Eq CompileError where
    (CompileError a _ c) == (CompileError a' _ c') = a == a'

--instance Show Error where

instance Eq Error where
    (SyntaxError _) == (SyntaxError _) = True
    (NameError _ _ _) == (NameError _ _ _) = True
    (VarUnbound _) == (VarUnbound _) = True
    (Conflicting _ _) == (Conflicting _ _) = True
    (TooMuchArgs _ _ _) == (TooMuchArgs _ _ _) = True
    (TypeError _ _ _) == (TypeError _ _ _) = True
    (PatternException _) == (PatternException _) = True
    _ == _ = False

-- compile-time errors
nameError x y = CompileError "NameError" (position x) ("Conflicting definitions with " ++ (show y))
varUnbound x = CompileError "NameError" (position x) ("Variable " ++ (show x) ++ "unbound")

--runtime exceptions
functionException pos opName expected args
        | (length args) > expected = tooMuchArguments pos opName expected (length args)
        | otherwise = typeException pos opName args
nameException pos opName = CompileError "NameError" pos ("function " ++ (name opName) ++ " not found")
typeException pos opName list = CompileError "TypeError" pos ("unsupported types for " ++ opName ++ ": " ++ (foldl1 (\x y -> x ++ " and " ++ y) (map show list)))
tooMuchArguments pos opName expected got =
    CompileError "TypeError" pos (opName ++ " takes at most " ++ (show expected) ++ " arguments (" ++ (show got) ++ " given)")
patternException expr = CompileError "PatternException" (position expr) "Couldn't match arguments against pattern"
