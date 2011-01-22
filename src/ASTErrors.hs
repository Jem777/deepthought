module ASTErrors where

import AST

functionError pos opName expected args
        | (toInteger (length args)) > expected = TooMuchArgs pos opName expected (toInteger (length args))
        | otherwise = TypeException pos opName args

