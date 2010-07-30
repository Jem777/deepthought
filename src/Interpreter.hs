module Main where

import Data.Either (either)

import Compile
import Parser
import Types
import StdLib
import Expr

main = runEitherErr (run "3+3") >>= either print print

run code = f ((testparse expression) "" code)
    where
    f (Right x) = eval testEmptyState x
    f (Left x) = goLeft [formatError x]
