module CompileAST where

import AST
import ASTErrors
import qualified Types
import StdLib
import State
import CompilerErrors
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
    toAst (Types.Lambda args body) = (\a -> Lambda . (f a)) <$> mapM toAst args <*> toAst body
        where f args body pos pattern = saveArgs args pattern >> (eval body)
    --f :: [ASTDatatype] -> ASTDatatype -> SourcePos -> [ASTDatatype] -> Eval ASTDatatype

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
