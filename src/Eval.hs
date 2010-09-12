module Eval where

import Control.Monad
import Control.Applicative
import Types
import Misc
import Errors

-- typeclasses for evaluation

class Evaluate a where
    eval :: State -> a -> EitherErr IO Datatype

class Execute a where
    exec :: a -> SourcePos -> State -> [Datatype] -> EitherErr IO Datatype

-- monadic type for evaluation

newtype (Monad m) => EitherErr m a = EitherErr { runEitherErr :: m (Either [CompileError] a)}
newtype EitherList a = EitherList {runEitherList :: Either [CompileError] [a]}

-- instances

instance Monad EitherList where
    return = EitherList . Right . (:[])
    --x >>= f = EitherList (either Left (runEitherList . map . f) (runEitherList x))
    x >>= f = EitherList (either Left (runEitherList . f . head) (runEitherList x))

instance Functor EitherList where
    fmap f x = EitherList (either Left (Right . (map f)) (runEitherList x))

instance (Monad m) => Monad (EitherErr m) where
    return = EitherErr . return . Right
    x >>= f = EitherErr (runEitherErr x >>= either (return . Left) (runEitherErr . f))

instance (Monad m) => MonadPlus (EitherErr m) where
    mzero = (EitherErr . return . Left) []
    mplus x y = EitherErr ((\a -> either (\b -> either (Left . (++b)) Right a) Right) `liftM` runEitherErr y `ap` runEitherErr x)

instance (Monad m) => Functor (EitherErr m) where
    fmap f x = EitherErr (runEitherErr x >>= either (return . Left) (return . Right . f))

instance (Monad m) => Applicative (EitherErr m) where
    pure = return
    (<*>) = ap
