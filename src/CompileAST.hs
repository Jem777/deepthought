module CompileAST where

import AST
import qualified Types
import State
import Eval

import Data.Either
import Control.Applicative

class AST a where
    toAst :: a -> Compiler ASTDatatype

instance AST Types.Datatype where
    toAst (Types.Number a) = return (Number a)
    toAst (Types.Float a) = return (Float a)
    toAst (Types.String a) = return (String a)
    toAst (Types.Char a) = return (Char a)
    toAst (Types.Atom a) = return (Atom a)
    toAst (Types.List a) = mapM toAst a >>= return . List
    toAst (Types.Vector a) = mapM toAst a >>= return . Vector
    toAst (Types.Lambda args body) = (\a -> Lambda . (f a)) <$> (mapM toAst args >>= mapM addVarState) <*> (toAst body >>= checkVarState)
        where f args body pos pattern = saveArgs args pattern >> (eval body)

instance AST Types.Expression where
    toAst (Types.Variable pos name) = return (Variable pos name) --resolveVariable name
    toAst (Types.Operator pos m f) = resolveFunction (m,f) >>= return . uncurry (Operator pos)
    toAst (Types.Application pos op args) = toAst op >>= \operator -> (mapM toAst args) >>= return . (Application operator pos)
    toAst (Types.Datatype pos datatype) = toAst datatype
    toAst Types.Wildcard = (return . Atom) "__wildcard__"
    toAst _ = return (Atom "asdf")

--instance AST Types.Definition where
--    toAst (Types.Definition)

--addDefinition modName (Types.Definition pos name patterns) = addFunction modName name (map addPattern patterns)

--[([Expression], Expression, Expression, [Definition])]
--addPattern (varList, guard, body, inlineFun)

addVarState :: ASTDatatype -> Compiler ASTDatatype
addVarState (Variable pos name) = addVariable name >> return (Variable pos name)
addVarState (Application op pos args) = mapM addVarState args >> return (Application op pos args)
addVarState (List a) = mapM addVarState a >> return (List a)
addVarState (Vector a) = mapM addVarState a >> return (Vector a)
addVarState x = return x

checkVarState :: ASTDatatype -> Compiler ASTDatatype
checkVarState (Variable pos name) = elemVariable name >> return (Variable pos name)
checkVarState (Application op pos args) = mapM checkVarState args >> return (Application op pos args)
checkVarState (List a) = mapM checkVarState a >> return (List a)
checkVarState (Vector a) = mapM checkVarState a >> return (Vector a)
checkVarState x = return x
