module StdLib where

import Types
import Errors
import Data.List
import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import Control.Monad (ap, mapM)

-- the functions



--add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
--add _ [x] = goRight (Lambda foo bar)
add _ [List a, List b] = goRight (List (a ++ b))
add _ [Tupel a, Tupel b] = goLeft [CompileError "TypeError" testEmptyPos ""] --tupelAdd a b
add _ [Number a, Number b] = goRight (Number (a + b))
add _ [Float a, Number b] = goRight (Float (a + (fromInteger b)))
add _ [Number a, Float b] = goRight (Float ((fromInteger a) + b))
add _ [Float a, Float b] = goRight (Float (a + b))
add _ [String a, String b] = goRight (String (a ++ b))
add pos x | (length x) > 2 = goLeft [tooMuchArguments pos "+" 2 (length x)]
          | otherwise = goLeft [typeException pos "+" x]

--sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ [List a, List b] = goRight (List (a \\ b)) -- has the be evaled first
sub _ [Tupel a, Tupel b] = goLeft [CompileError "TypeError" testEmptyPos ""]
sub _ [Number a, Number b] = goRight (Number (a - b))
sub _ [Float a, Number b] = goRight (Float (a - (fromInteger b)))
sub _ [Number a, Float b] = goRight (Float ((fromInteger a) - b))
sub _ [Float a, Float b] = goRight (Float (a - b))
sub _ [String a, String b] = goRight (String (a \\ b))
sub pos x | (length x) > 2 = goLeft [tooMuchArguments pos "-" 2 (length x)]
          | otherwise = goLeft [typeException pos "-" x]

neg :: SourcePos -> [Datatype] -> IO (Either [CompileError] Datatype)
neg _ [Number a] = goRight (Number (negate a))
neg _ [Float a] = goRight (Float (negate a))
neg pos x | (length x) > 1 = goLeft [tooMuchArguments pos "negate" 1 (length x)]
          | otherwise = goLeft [typeException pos "negate" x]

mul _ [List a, Number b] = goRight (List (concat (replicate (fromInteger b) a)))
mul _ [Tupel a, Number b] = goLeft [CompileError "TypeError" testEmptyPos ""] -- has to be evaled first
mul _ [Number a, Number b] = goRight (Number (a * b))
mul _ [Float a, Number b] = goRight (Float (a * (fromInteger b)))
mul _ [Number a, Float b] = goRight (Float ((fromInteger a) * b))
mul _ [Float a, Float b] = goRight (Float (a * b))
mul _ [String a, Number b] = goRight (String (concat (replicate (fromInteger b) a)))
mul pos x | (length x) > 2 = goLeft [tooMuchArguments pos "*" 2 (length x)]
          | otherwise = goLeft [typeException pos "*" x]

division _ [Number a, Number b] = goRight (Float ((fromInteger a) / (fromInteger b)))
division _ [Float a, Number b] = goRight (Float (a / (fromInteger b)))
division _ [Number a, Float b] = goRight (Float ((fromInteger a) / b))
division _ [Float a, Float b] = goRight (Float (a / b))
division pos x | (length x) > 2 = goLeft [tooMuchArguments pos "/" 2 (length x)]
               | otherwise = goLeft [typeException pos "/" x]

printf _ [Number a] = do_io print a
printf _ [String a] = do_io print a
printf _ [Float a] = do_io print a
printf _ [Char a] = do_io print a
printf _ [Atom a] = do_io putStrLn a
printf pos x | (length x) > 1 = goLeft [tooMuchArguments pos "print" 1 (length x)]
             | otherwise = goLeft [typeException pos "print" x]

printStr _ [String a] = do_io putStrLn a
printStr pos x | (length x) > 1 = goLeft [tooMuchArguments pos "printStr" 1 (length x)]
             | otherwise = goLeft [typeException pos "printStr" x]

do_io f x = f x >> goRight (Atom "@ok")
--listconstructor ()


eval state (Application pos fun args) = apply (translateFun fun) state pos fun args
eval _ (Datatype _ x) = goRight x
eval state (Variable pos str)
    | getVariable state (Variable pos str) == Nothing = goLeft [CompileError "asdf" pos ("variable " ++ str ++ " unbound")]
    | otherwise = (goRight . fromJust) (getVariable state (Variable pos str))
eval a b = goRight (Atom "foo")


apply Nothing _ p fun _ = goLeft [nameException p fun]
apply (Just f) state pos fun args = (apply2 pos) state f args

apply2 pos state fun args = (mapM evaluate args) >>= help (fun pos)
    where
    evaluate x = (return x) >>= (eval state)
    help f l
        | (lefts l == []) = f (rights l)
        | otherwise = goLeft (concat (lefts l))

translateFun fun = lookup (value fun) functionList

pureFunctions = [("+", add), ("-", sub), ("neg", neg), ("*", mul), ("/", division)]
impureFunctions = [("print", printf), ("printStr", printStr)]
functionList = (pureFunctions ++ impureFunctions)
--functionList = [("neg", neg)]

-- internal functions

goRight = return . Right
goLeft = return . Left

tupelAdd [] [] = Right (Tupel [])
tupelAdd a b | (length a) == (length b) = Right (Tupel (a ++ b))
             | otherwise = Left (CompileError "TypeError" testEmptyPos "length of the tupels don't match")


--reportTypeError :: String -> [Datatype] -> CompileError
