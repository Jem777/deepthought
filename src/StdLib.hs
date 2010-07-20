module StdLib where

import Types
import Errors
import Data.List


eval :: State -> Expression -> Either [CompileError] Datatype
eval state (Application pos fun args) = apply (translate_fun fun) state pos fun args
eval a b = Right (Atom "foo")

-- the functions

add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
add _ (List a) (List b) = Right (List (a ++ b))
add _ (Tupel a) (Tupel b) = Left [CompileError "TypeError" testEmptyPos ""] --tupelAdd a b
add _ (Number a) (Number b) = Right (Number (a + b))
add _ (Float a) (Number b) = Right (Float (a + (fromInteger b)))
add _ (Number a) (Float b) = Right (Float ((fromInteger a) + b))
add _ (Float a) (Float b) = Right (Float (a + b))
add _ (String a) (String b) = Right (String (a ++ b))
add pos x y = Left [typeError pos "+" x y]

sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ (List a) (List b) = Right (List (a \\ b)) -- has the be evaled first
sub _ (Tupel a) (Tupel b) = Left [CompileError "TypeError" testEmptyPos ""]
sub _ (Number a) (Number b) = Right (Number (a - b))
sub _ (Float a) (Number b) = Right (Float (a - (fromInteger b)))
sub _ (Number a) (Float b) = Right (Float ((fromInteger a) - b))
sub _ (Float a) (Float b) = Right (Float (a - b))
sub _ (String a) (String b) = Right (String (a \\ b))
sub pos x y = Left [typeError pos "-" x y]

neg :: SourcePos -> Datatype -> Either [CompileError] Datatype
neg _ (Number a) = Right (Number (negate a))
neg _ (Float a) = Right (Float (negate a))
neg pos _ = Left [CompileError "TypeError" pos "asdf"]

mul :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
mul _ (List a) (Number b) = Right (List (concat (replicate (fromInteger b) a)))
mul _ (Tupel a) (Number b) = Left [CompileError "TypeError" testEmptyPos ""] -- has to be evaled first
mul _ (Number a) (Number b) = Right (Number (a * b))
mul _ (Float a) (Number b) = Right (Float (a * (fromInteger b)))
mul _ (Number a) (Float b) = Right (Float ((fromInteger a) * b))
mul _ (Float a) (Float b) = Right (Float (a * b))
mul _ (String a) (Number b) = Right (String (concat (replicate (fromInteger b) a)))
mul pos x y = Left [typeError pos "*" x y]

division :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
division _ (Number a) (Number b) = Right (Float ((fromInteger a) / (fromInteger b)))
division _ (Float a) (Number b) = Right (Float (a / (fromInteger b)))
division _ (Number a) (Float b) = Right (Float ((fromInteger a) / b))
division _ (Float a) (Float b) = Right (Float (a / b))
division pos x y = Left [typeError pos "/" x y]

--listconstructor ()

apply Nothing _ p f _ = Left [CompileError "NameError" p ("function " ++ (value f) ++ "not found")]
apply (Just f) state pos fun (arg1:arg2:args) = help (evaluate arg1) (evaluate arg2)
    where
        evaluate x = eval state x
        help (Left x) (Left y) = Left (x ++ y)
        help (Left x) _ = Left x
        help _ (Left y) = Left y
        help (Right x) (Right y) = f pos x y

io_wrapper (Right x) = Right (return x)
io_wrapper (Left x) = Left x

translate_fun fun = lookup (value fun) fun_list

fun_list = [("+", add), ("-", sub), ("*", mul), ("/", division)]
-- internal functions

tupelAdd [] [] = Right (Tupel [])
tupelAdd a b | (length a) == (length b) = Right (Tupel (a ++ b))
             | otherwise = Left (CompileError "TypeError" testEmptyPos "length of the tupels don't match")


--reportTypeError :: String -> [Datatype] -> CompileError
