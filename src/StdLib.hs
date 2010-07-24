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


goRight = return . Right
goLeft = return . Left

--add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
add _ (List a) (List b) = goRight (List (a ++ b))
add _ (Tupel a) (Tupel b) = goLeft [CompileError "TypeError" testEmptyPos ""] --tupelAdd a b
add _ (Number a) (Number b) = goRight (Number (a + b))
add _ (Float a) (Number b) = goRight (Float (a + (fromInteger b)))
add _ (Number a) (Float b) = goRight (Float ((fromInteger a) + b))
add _ (Float a) (Float b) = goRight (Float (a + b))
add _ (String a) (String b) = goRight (String (a ++ b))
add pos x y = goLeft [typeException pos "+" x y]

--sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ (List a) (List b) = goRight (List (a \\ b)) -- has the be evaled first
sub _ (Tupel a) (Tupel b) = goLeft [CompileError "TypeError" testEmptyPos ""]
sub _ (Number a) (Number b) = goRight (Number (a - b))
sub _ (Float a) (Number b) = goRight (Float (a - (fromInteger b)))
sub _ (Number a) (Float b) = goRight (Float ((fromInteger a) - b))
sub _ (Float a) (Float b) = goRight (Float (a - b))
sub _ (String a) (String b) = goRight (String (a \\ b))
sub pos x y = goLeft [typeException pos "-" x y]

neg :: SourcePos -> Datatype -> Either [CompileError] Datatype
neg _ (Number a) = Right (Number (negate a))
neg _ (Float a) = Right (Float (negate a))
neg pos _ = Left [CompileError "TypeError" pos "asdf"]

mul _ (List a) (Number b) = goRight (List (concat (replicate (fromInteger b) a)))
mul _ (Tupel a) (Number b) = goLeft [CompileError "TypeError" testEmptyPos ""] -- has to be evaled first
mul _ (Number a) (Number b) = goRight (Number (a * b))
mul _ (Float a) (Number b) = goRight (Float (a * (fromInteger b)))
mul _ (Number a) (Float b) = goRight (Float ((fromInteger a) * b))
mul _ (Float a) (Float b) = goRight (Float (a * b))
mul _ (String a) (Number b) = goRight (String (concat (replicate (fromInteger b) a)))
mul pos x y = goLeft [typeException pos "*" x y]

division _ (Number a) (Number b) = goRight (Float ((fromInteger a) / (fromInteger b)))
division _ (Float a) (Number b) = goRight (Float (a / (fromInteger b)))
division _ (Number a) (Float b) = goRight (Float ((fromInteger a) / b))
division _ (Float a) (Float b) = goRight (Float (a / b))
division pos x y = goLeft [typeException pos "/" x y]

printf :: SourcePos -> Datatype -> Datatype -> IO (Either [CompileError] Datatype)
printf _ (Number a) _ = do_io print a
printf _ (String a) _ = do_io print a
printf _ (Float a) _ = do_io print a
printf _ (Char a) _ = do_io print a
printf _ (Atom a) _ = do_io print a
printf pos x y = (return . Left) [typeException pos "print" x y]

do_io f x = f x >> return (Right (Atom "@true"))
--listconstructor ()


eval state (Application pos fun args) = apply (translate_fun fun) state pos fun args
eval _ (Datatype _ x) = (return . Right) x
eval a b = (return . Right) (Atom "foo")


apply Nothing _ p fun _ = (return . Left) [nameException p fun]
apply (Just f) state pos fun (arg1:arg2:args) = (apply2 pos) state f arg1 arg2

apply2 pos state fun arg1 arg2 = (evaluate arg2) >>= (\y -> (evaluate arg1) >>= (\x -> g x y))
    where
    g a1 a2 = help (fun pos) a1 a2 --((return help) >>= (fun pos))
    evaluate x = (return x) >>= (eval state)
    help :: (Datatype -> Datatype -> (IO (Either [CompileError] Datatype))) -> Either [CompileError] Datatype -> Either [CompileError] Datatype -> IO (Either [CompileError] Datatype)
    help f (Right x) (Right y) = f x y
    help _ (Left x) (Left y) = (return . Left) (x ++ y)
    help _ (Left x) _ = (return . Left) x
    help _ _ (Left x) = (return . Left) x

--translate_fun fun = lookup (value fun) fun_list
translate_fun fun = lookup (value fun) functionList

pureFunctions = [("+", add), ("-", sub), ("*", mul), ("/", division)]
impureFunctions = [("print", printf)]
functionList = (pureFunctions ++ impureFunctions)

-- internal functions

tupelAdd [] [] = Right (Tupel [])
tupelAdd a b | (length a) == (length b) = Right (Tupel (a ++ b))
             | otherwise = Left (CompileError "TypeError" testEmptyPos "length of the tupels don't match")


--reportTypeError :: String -> [Datatype] -> CompileError
