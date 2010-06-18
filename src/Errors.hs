module Errors where

import Types

nameError x y = CompileError "NameError" (position x) ("Conflicting definitions with " ++ (show y))
varUnbound x = CompileError "NameError" (position x) ("Variable " ++ (show x) ++ "unbound")
