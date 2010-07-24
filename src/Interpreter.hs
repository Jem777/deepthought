module Interpreter where

import Compile
import Parser
import Types
import StdLib
import Expr

main = run "3 + 3" >>= print

run code = f ((testparse expression) "" code)
    where
    f (Right x) = eval testEmptyState x
    f (Left x) =  return (Left [formatError x])
