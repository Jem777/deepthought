module AST where

import qualified Text.ParserCombinators.Parsec.Pos as P (SourcePos)
import Control.Monad
import Control.Applicative
import Data.List (intercalate)

data RuntimeError =
    TooMuchArgs SourcePos String Integer Integer
    | TypeError String SourcePos String
    | PatternException SourcePos
    | TypeException SourcePos String [ASTDatatype]

instance Eq RuntimeError where
    (TooMuchArgs _ _ _ _) == (TooMuchArgs _ _ _ _) = True
    (TypeError _ _ _) == (TypeError _ _ _) = True
    (PatternException _) == (PatternException _) = True
    _ == _ = False

newtype EitherErr a = EitherErr { runEitherErr :: IO (Either [RuntimeError] a)}

instance Monad EitherErr where
    return = EitherErr . return . Right
    x >>= f = EitherErr (runEitherErr x >>= either (return . Left) (runEitherErr . f))

instance MonadPlus EitherErr where
    mzero = (EitherErr . return . Left) []
    mplus x y = EitherErr ((\a -> either (\b -> either (Left . (++b)) Right a) Right) `liftM` runEitherErr y `ap` runEitherErr x)

instance Functor EitherErr where
    fmap f x = EitherErr (runEitherErr x >>= either (return . Left) (return . Right . f))

type SourcePos = P.SourcePos

data ASTDatatype =
    List [ASTDatatype]
    | Vector [ASTDatatype]
    | Number Integer
    | Float Double
    | String String
    | Char Char
    | Atom String
    | Operator SourcePos String String
    | Variable SourcePos String String
    | Application ASTDatatype SourcePos [ASTDatatype]
    | Lambda (SourcePos -> [ASTDatatype] -> EitherErr ASTDatatype)

{-
eval (Application (Lambda l) pos args) = l pos args
eval (Application _ pos _) = goLeft []
eval x = goRight x
-}

instance Eq ASTDatatype where
    (List a) == (List b) = a == b
    (Vector a) == (Vector b) = a == b
    (Number a) == (Number b) = a == b
    (Float a) == (Float b) = a == b
    (String a) == (String b) = a == b
    (Char a) == (Char b) = a == b
    (Atom a) == (Atom b) = a == b
    (Application a _ c) == (Application a' _ c') = a == a' && c == c'
    (Lambda _) == (Lambda _) = True
    _ == _ = False
instance Show ASTDatatype where
    show (List a) = show a
    show (Vector a) = "(" ++ (intercalate "," (map show a)) ++ ")"
    show (Number a) = show a
    show (Float a) = show a
    show (String a) = show a
    show (Char a) = show a
    show (Atom a) = a
    show (Lambda _) = "<Lambda>"

