module Interpreter where

import Compile
import Parser
import Types
import StdLib
import Expr

main = putStrLn "asdf"

run code = f ((testparse expression) "" code)
    where 
    f (Right x) = eval testEmptyState x
    f (Left x) =  return (Left [formatError x])
