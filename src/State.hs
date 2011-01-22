module State
    --(Compiler, addModule, addFunction, getFunction, importModule, setExports)
    where

import AST
import StdLib (builtins)
import CompilerErrors

import Control.Monad.State
import Data.List (reverse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)


data CompilerState = CompilerState Tree String [Import] [String]
newtype CompilerMonad a = CompilerMonad {runCompilerMonad :: Either [CompilerError] a}
type Import = (String, (String, [String])) --alias, module name and the functions
type Module = Map String ASTDatatype
type Tree = Map String Module
type CompilerT = State CompilerState
type Compiler = StateT CompilerState CompilerMonad

addModule :: String -> Module -> Compiler ()
addModule m funMap = modify (changeTree (Map.insert m funMap))
addFunction (m, f) lambda = modify (changeTree (Map.adjust (\funMap -> Map.insert f lambda funMap) m))
{-
resolveFunction :: (String, String) -> Compiler (Maybe (String, String))
resolveFunction = gets . internResolve
callFunction :: (String, String) -> Compiler (Maybe ASTDatatype)
callFunction = gets . internLambda
toLambda :: (String, String) -> Compiler (Maybe ASTDatatype)
toLambda = gets . internCallDirect
-}
resolveFunction = helper3 internResolve
callFunction = helper3 internLambda
toLambda = helper3 internCallDirect

helper1 f = maybe (Left [FoobarError]) (Right . f)
helper2 = maybe (Left [FoobarError]) Right

helper :: Compiler (CompilerMonad a) -> Compiler a
helper = StateT . (\f s -> f s >>= \(a, s) -> a >>= \x -> return (x,s)) . runStateT

helper3 f a = helper . gets $ CompilerMonad . helper2 . (f a)


importModule imports = modify (changeImports (const imports))
setExports exports = modify (changeExports (const exports))

newState = CompilerState (Map.fromList [("StdLib", Map.fromList builtins)]) "" [("", ("StdLib", map fst builtins))] []

changeTree f (CompilerState t m i e) = CompilerState (f t) m i e
getTree (CompilerState t _ _ _) = t
changeModule f (CompilerState t m i e) = CompilerState t (f m) i e
getModule (CompilerState _ m _ _) = m
changeImports f (CompilerState t m i e) = CompilerState t m (f i) e
getImports (CompilerState _ _ i _) = i
changeExports f (CompilerState t m i e) = CompilerState t m i (f e)
getExports (CompilerState _ _ _ e) = e

internResolve (m,f) = (>>= (return . ((flip (,)) f) . fst . snd)) . listToMaybe . filter (any (==f) . snd . snd) . filter ((==m) . fst) . getImports
internLambda (m,f) = (=<<) (Map.lookup f) . Map.lookup m . getTree
internCallDirect a s = internResolve a s >>= (flip internLambda) s

instance Monad CompilerMonad where
    return = CompilerMonad . Right
    x >>= f = CompilerMonad (either Left (runCompilerMonad . f) (runCompilerMonad x))

{-
instance MonadPlus CompilerMonad where
    mzero = (CompilerMonad . Left) []
    mplus x y = CompilerMonad ((\a -> either (\b -> either (Left . (++b)) Right a) Right) `liftM` runEitherErr y `ap` runEitherErr x)
-}
instance Functor CompilerMonad where
    fmap f x = CompilerMonad (either Left (Right . f) (runCompilerMonad x))
