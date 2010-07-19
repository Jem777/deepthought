module Errors where

import Types

-- compile-time errors
nameError x y = CompileError "NameError" (position x) ("Conflicting definitions with " ++ (show y) ++ "\n")
varUnbound x = CompileError "NameError" (position x) ("Variable " ++ (show x) ++ "unbound\n")

--runtime exceptions
typeError pos opName x y = CompileError "TypeError" pos ("unsupported types for " ++ opName ++ ": " ++ (show x) ++ " and " ++ (show y) ++ "\n")
