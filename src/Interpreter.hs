module Main where

import Data.Either (either)
import Control.Monad.State

--import Compile
import Parser
--import Types
--import StdLib
import Expr
import Eval
import CompileAST
import ASTErrors
import CompilerErrors
import AST
import State
--import Errors

--main = runEitherErr (run "3+3") >>= either print print
{-
main = getLine >>= run

run code = either
    (\x -> print [formatError x])
    (\x -> runEitherErr (eval testEmptyState x >>= prettyShow testEmptyState) >>=
        either print putStrLn)
    ((testparse expression) "" code)

-}

testrun code = runEvalMonad (runAST newState ((testparse expression) "" code)) >>= either print print

buildAST state = either (CompilerMonad . Left . (:[]) . ParseError) ((flip evalStateT state) . toAst)
runAST state = (>>= flip evalStateT (transferState state) . eval) . transferMonad . buildAST state

debugAST a = either print print . runCompilerMonad . buildAST a
