module State
    (addModule, addFunction, resolveFunction, callFunction, toLambda, resolveVariable, addVariable, transferState, newState, cleanState, importModule, setExports)
    where

import AST
import StdLib (builtins)
import CompilerErrors
import Misc
import Eval

import Control.Monad.State
import Data.List (reverse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Control.Applicative


evalS :: Compiler a -> CompilerState -> CompilerMonad a
evalS a b = evalStateT a b
runS :: Compiler a -> CompilerState -> CompilerMonad (a, CompilerState)
runS a b = runStateT a b

addModule :: String -> Module -> Compiler ()
addModule m funMap = modify (changeTree (Map.insert m funMap))
addFunction (m, f) lambda = modify (changeTree (Map.adjust (\funMap -> Map.insert f lambda funMap) m))
resolveFunction = call internResolve FoobarError
callFunction = call internLambda FoobarError
toLambda = call internCallDirect FoobarError
resolveVariable = call (\v -> Map.lookup v . getVariables) FoobarError
addVariable key value = modify (changeVariables (Map.insert key value))

maybeError f = CompilerMonad . maybe (Left [f]) Right
reduceMonad :: Compiler (CompilerMonad a) -> Compiler a
reduceMonad = StateT . (\f s -> f s >>= \(a, s) -> a >>= \x -> return (x,s)) . runStateT
call f g a = reduceMonad . gets $ maybeError g . (f a)

importModule imports = modify (changeImports (const imports))
setExports exports = modify (changeExports (const exports))

transferState state = EvalState (getTree state) Map.empty
newState = CompilerState (Map.singleton "StdLib" (Map.fromList (map f builtins))) "" (Map.singleton "" ("StdLib", map fst builtins)) [] Map.empty
    where f (a,b) = (a, Lambda (\pos args -> mapM eval args >>= b pos))
cleanState (CompilerState t _ _ _ _) = CompilerState t "" (Map.singleton "" ("StdLib", map fst builtins)) [] Map.empty

changeTree f (CompilerState t m i e v) = CompilerState (f t) m i e v
getTree (CompilerState t _ _ _ _) = t
changeModule f (CompilerState t m i e v) = CompilerState t (f m) i e v
getModule (CompilerState _ m _ _ _) = m
changeImports f (CompilerState t m i e v) = CompilerState t m (f i) e v
getImports (CompilerState _ _ i _ _) = i
changeExports f (CompilerState t m i e v) = CompilerState t m i (f e) v
getExports (CompilerState _ _ _ e _) = e
changeVariables f (CompilerState t m i e v) = CompilerState t m i e (f v)
getVariables (CompilerState _ _ _ _ v) = v

internResolve (m,f) = fmap snd . listToMaybe . Map.toList . Map.mapMaybeWithKey mapper . getImports
    where mapper k a = if k == m && elem f (snd a) then Just (fst a, f) else Nothing
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

instance (Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap
