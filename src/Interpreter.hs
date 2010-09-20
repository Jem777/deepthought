module Main where

import Data.Either (either)

import Compile
import Parser
import Types
import StdLib
import Expr
import Eval
import Errors

--main = runEitherErr (run "3+3") >>= either print print
main = getLine >>= run

run code = either
    (\x -> print [formatError x])
    (\x -> runEitherErr (eval testEmptyState x >>= prettyShow testEmptyState) >>=
        either print putStrLn)
    ((testparse expression) "" code)
