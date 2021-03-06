module CompileAST (AST, toAst, checkVarState) where

import AST
import qualified Types
import State
import Eval
import CompilerErrors
import Misc

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
    toAst (Types.Lambda args body) = createLambda <$> (mapM toAst args >>= mapM addVarState) <*> (toAst body >>= checkVarState)

instance AST Types.Expression where
    toAst (Types.Variable pos name) = return (Variable pos name) --resolveVariable name
    toAst (Types.Operator pos m f) = resolveFunction (NameError pos (funcName m f)) (m,f) >>= return . uncurry (Operator pos)
    toAst (Types.Application pos op args) = toAst op >>= \operator -> (mapM toAst args) >>= return . (Application operator pos)
    toAst (Types.Datatype pos datatype) = toAst datatype
    toAst Types.Wildcard = (return . Atom) "__wildcard__"
    toAst _ = return (Atom "asdf")

instance AST Types.Definition where
    toAst (Types.Definition pos name patternList) = createDefinition <$> mapM f patternList
        where f (var, guard, body, inline) = (\a b c d -> (a,b,c,d)) <$> mapM toAst var <*> toAst guard <*> toAst body <*> mapM toAst inline
    -- Definition SourcePos String [([Expression], Expression, Expression, [Definition])]
    -- Arguments: Position, FunctionName, [(Variables, Guard, Body, InlineFunctions)]

instance AST Types.InlineFunction where
    toAst (Types.InlineFunction pos patternList) = createDefinition <$> mapM f patternList
        where f (var, guard, body) = (\a b c -> (a,b,c,[])) <$> mapM toAst var <*> toAst guard <*> toAst body

--addDefinition modName (Types.Definition pos name patterns) = addFunction modName name (map addPattern patterns)

--[([Expression], Expression, Expression, [Definition])]
--addPattern (varList, guard, body, inlineFun)

createLambda args body = createHelper args (Atom "@true") body
createHelper args guard body = createFunction args guard body []
createFunction args guard body inline = createDefinition [(args, guard, body, inline)]
createDefinition function = Lambda (\pos pattern -> iterateTerms function pos pattern)

seqPattern list = list

addVarState :: ASTDatatype -> Compiler ASTDatatype
addVarState (Variable pos name) = addVariable name >> return (Variable pos name)
addVarState (Application op pos args) = mapM addVarState args >> return (Application op pos args)
addVarState (List a) = mapM addVarState a >> return (List a)
addVarState (Vector a) = mapM addVarState a >> return (Vector a)
addVarState x = return x

checkVarState :: ASTDatatype -> Compiler ASTDatatype
checkVarState (Variable pos name) = elemVariable (NameError pos name) name >> return (Variable pos name)
checkVarState (Application op pos args) = mapM checkVarState args >> return (Application op pos args)
checkVarState (List a) = mapM checkVarState a >> return (List a)
checkVarState (Vector a) = mapM checkVarState a >> return (Vector a)
checkVarState x = return x

funcName "" f = f
funcName m f = m ++ moduleSep ++ f
