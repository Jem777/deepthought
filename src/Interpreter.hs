module Main where

import Data.Either (either)
import Control.Monad.State

import Parser
import Expr
import Eval
import CompileAST
import ASTErrors
import CompilerErrors
import AST
import State

main = getLine >>= testrun

testrun code = runEvalMonad (runAST newState ((testparse expression) "" code)) >>= either print print

buildAST state = either (CompilerMonad . Left . (:[]) . ParseError) ((flip evalStateT state) . (>>= checkVarState) . toAst)
--runAST state = (>>= flip evalStateT (transferState state) . eval ) . transferMonad . buildAST state
runAST state = flip evalStateT (transferState state) <=< return . eval <=< transferMonad . buildAST state

debugAST a = either print print . runCompilerMonad . buildAST a
