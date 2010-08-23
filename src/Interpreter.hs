module Main where

import Data.Either (either)

import Compile
import Parser
import Types
import StdLib
import Expr

--main = runEitherErr (run "3+3") >>= either print print
main = run "3+3"

run code = f ((testparse expression) "" code)
    where
    f (Right x) =
        runEitherErr (eval testEmptyState x >>= prettyShow testEmptyState) >>=
        either print putStrLn
    f (Left x) = print [formatError x]
