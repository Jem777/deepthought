module Errors where

import Types

-- compile-time errors
nameError x y = CompileError "NameError" (position x) ("Conflicting definitions with " ++ (show y))
varUnbound x = CompileError "NameError" (position x) ("Variable " ++ (show x) ++ "unbound")

--runtime exceptions
nameException pos opName = CompileError "NameError" pos ("function " ++ (value opName) ++ " not found")
typeException :: SourcePos -> String -> [Datatype] -> CompileError
typeException pos opName list = CompileError "TypeError" pos ("unsupported types for " ++ opName ++ ": " ++ (foldl1 (\x y -> x ++ " and " ++ y) (map show list)))
tooMuchArguments pos opName exspected got =
    CompileError "TypeError" pos (opName ++ "takes at most " ++ (show exspected) ++ " arguments (" ++ (show got) ++ " given)")
