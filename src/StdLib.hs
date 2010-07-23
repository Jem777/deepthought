module StdLib where

import Types
import Errors
import Data.List
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)

{-
eval :: State -> Expression -> Either [CompileError] (m Datatype)
--eval state (Application pos fun args) = apply (translate_fun fun) state pos fun args
eval a b = io_out (Atom "foo")
-}
-- the functions

--add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
add _ (List a) (List b) = io_out (List (a ++ b))
add _ (Tupel a) (Tupel b) = Left [CompileError "TypeError" testEmptyPos ""] --tupelAdd a b
add _ (Number a) (Number b) = io_out (Number (a + b))
add _ (Float a) (Number b) = io_out (Float (a + (fromInteger b)))
add _ (Number a) (Float b) = io_out (Float ((fromInteger a) + b))
add _ (Float a) (Float b) = io_out (Float (a + b))
add _ (String a) (String b) = io_out (String (a ++ b))
add pos x y = Left [typeException pos "+" x y]

--sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ (List a) (List b) = io_out (List (a \\ b)) -- has the be evaled first
sub _ (Tupel a) (Tupel b) = Left [CompileError "TypeError" testEmptyPos ""]
sub _ (Number a) (Number b) = io_out (Number (a - b))
sub _ (Float a) (Number b) = io_out (Float (a - (fromInteger b)))
sub _ (Number a) (Float b) = io_out (Float ((fromInteger a) - b))
sub _ (Float a) (Float b) = io_out (Float (a - b))
sub _ (String a) (String b) = io_out (String (a \\ b))
sub pos x y = Left [typeException pos "-" x y]

neg :: SourcePos -> Datatype -> Either [CompileError] Datatype
neg _ (Number a) = Right (Number (negate a))
neg _ (Float a) = Right (Float (negate a))
neg pos _ = Left [CompileError "TypeError" pos "asdf"]

mul _ (List a) (Number b) = io_out (List (concat (replicate (fromInteger b) a)))
mul _ (Tupel a) (Number b) = Left [CompileError "TypeError" testEmptyPos ""] -- has to be evaled first
mul _ (Number a) (Number b) = io_out (Number (a * b))
mul _ (Float a) (Number b) = io_out (Float (a * (fromInteger b)))
mul _ (Number a) (Float b) = io_out (Float ((fromInteger a) * b))
mul _ (Float a) (Float b) = io_out (Float (a * b))
mul _ (String a) (Number b) = io_out (String (concat (replicate (fromInteger b) a)))
mul pos x y = Left [typeException pos "*" x y]

division _ (Number a) (Number b) = io_out (Float ((fromInteger a) / (fromInteger b)))
division _ (Float a) (Number b) = io_out (Float (a / (fromInteger b)))
division _ (Number a) (Float b) = io_out (Float ((fromInteger a) / b))
division _ (Float a) (Float b) = io_out (Float (a / b))
division pos x y = Left [typeException pos "/" x y]

printf _ (Number a) _ = putStrLn (show a)
--listconstructor ()


eval :: (Monad m) => State -> Expression -> m (Either [CompileError] Datatype)
--eval state (Application pos fun args) = apply (translate_fun fun) state pos fun args
eval a b = (return . Right) (Atom "foo")

{-
--apply :: Maybe (SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype) -> State -> SourcePos -> Expression -> [Expression] -> m (Either [CompileError] Datatype)
apply Nothing _ p fun _ = return (Left [nameException p fun])
apply (Just f) state pos fun (arg1:arg2:args) = (deMonad (f pos)) `fmap` (evaluate arg1) `ap` (evaluate arg2)
    where
        evaluate :: Expression -> m (Either [CompileError] Datatype)
        evaluate = eval state
-}

apply Nothing _ p fun _ = return (Left [nameException p fun])
apply (Just f) state pos fun (arg1:arg2:args) = (apply2 state f arg1 arg2)

apply2 state fun arg1 arg2 = fun `fmap` (fmap (eval state) arg1) `ap` (fmap (eval state) arg2)
{-
deMonad :: (Datatype -> Datatype -> Either [CompileError] Datatype)
    -> Either [CompileError] Datatype -- argument 1
    -> Either [CompileError] Datatype -- arg 2
    -> (m Either [CompileError] Datatype) -- output
deMonad _ (Left x) (Left y) = return (Left (x ++ y))
deMonad _ (Left x) _ = return (Left x)
deMonad _ _ (Left y) = return (Left y)
deMonad f (Right arg1) (Right arg2) = return $ f `fmap` arg1 `ap` arg2--(f `fmap` arg1 `ap` arg2)
-}
translate_fun fun = lookup (value fun) fun_list
io_out = Right

fun_io = [("print", printf)]
fun_list = [("+", add), ("-", sub), ("*", mul), ("/", division)]
-- internal functions

tupelAdd [] [] = Right (Tupel [])
tupelAdd a b | (length a) == (length b) = Right (Tupel (a ++ b))
             | otherwise = Left (CompileError "TypeError" testEmptyPos "length of the tupels don't match")


--reportTypeError :: String -> [Datatype] -> CompileError
