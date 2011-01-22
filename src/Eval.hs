module Eval where

import AST
import StdLib
import ASTErrors


eval (Application (Lambda l) pos args) = l pos args
eval (Application _ pos _) = goLeft []
eval x = goRight x
