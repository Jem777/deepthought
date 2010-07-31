module StdLib where

-- System Imports
import Data.List
import Data.Either (lefts, rights, either)
import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative ((<$>), (<*>))

-- Deepthought Imports
import Types
import Errors
import Misc

-- the functions



--add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
--add _ [x] = goRight (Lambda foo bar)
add _ _ [List a, List b] = goRight (List (a ++ b))
add pos state [Vector a, Vector b] = tuplef pos state add [a,b]
add _ _ [Number a, Number b] = goRight (Number (a + b))
add _ _ [Float a, Number b] = goRight (Float (a + (fromInteger b)))
add _ _ [Number a, Float b] = goRight (Float ((fromInteger a) + b))
add _ _ [Float a, Float b] = goRight (Float (a + b))
add _ _ [String a, String b] = goRight (String (a ++ b))
add pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos "+" 2 (length x)]
        | otherwise = goLeft [typeException pos "+" x]

--sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ _ [List a, List b] = goRight (List (a \\ b)) -- has the be evaled first
sub pos state [Vector a, Vector b] = tuplef pos state sub [a,b]
sub _ _ [Number a, Number b] = goRight (Number (a - b))
sub _ _ [Float a, Number b] = goRight (Float (a - (fromInteger b)))
sub _ _ [Number a, Float b] = goRight (Float ((fromInteger a) - b))
sub _ _ [Float a, Float b] = goRight (Float (a - b))
sub _ _ [String a, String b] = goRight (String (a \\ b))
sub pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos "-" 2 (length x)]
        | otherwise = goLeft [typeException pos "-" x]

neg _ _ [Number a] = goRight (Number (negate a))
neg _ _ [Float a] = goRight (Float (negate a))
neg pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "negate" 1 (length x)]
        | otherwise = goLeft [typeException pos "negate" x]

abs _ _ [Number a] | a > 0 = goRight (Number a) | otherwise = goRight (Number (negate a))
abs _ _ [Float a] = goRight (Float (negate a))
abs pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "negate" 1 (length x)]
        | otherwise = goLeft [typeException pos "negate" x]

mul _ _ [List a, Number b] = goRight (List (concat (replicate (fromInteger b) a)))
mul pos state [Vector a, Number b] = tupleMul pos state mul a (Number b)
mul _ _ [Number a, Number b] = goRight (Number (a * b))
mul _ _ [Float a, Number b] = goRight (Float (a * (fromInteger b)))
mul _ _ [Number a, Float b] = goRight (Float ((fromInteger a) * b))
mul _ _ [Float a, Float b] = goRight (Float (a * b))
mul _ _ [String a, Number b] = goRight (String (concat (replicate (fromInteger b) a)))
mul pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos "*" 2 (length x)]
        | otherwise = goLeft [typeException pos "*" x]

division _ _ [Number a, Number b] = goRight (Float ((fromInteger a) / (fromInteger b)))
division _ _ [Float a, Number b] = goRight (Float (a / (fromInteger b)))
division _ _ [Number a, Float b] = goRight (Float ((fromInteger a) / b))
division _ _ [Float a, Float b] = goRight (Float (a / b))
division pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos "/" 2 (length x)]
        | otherwise = goLeft [typeException pos "/" x]

listconstructor pos _ [a, List b] = goRight (List ((Datatype pos a):b))
listconstructor pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos ":" 2 (length x)]
        | otherwise = goLeft [typeException pos ":" x]


--printf pos state [List a] = eitherFold (++)
--printf _ state [List a] = evalArgs state a >>= return . (\y -> "(" ++ (intercalate "," y) ++ ")") . (mapM show)
printf _ state [List a] = pretty state a "[" "]" >>= do_io putStrLn
printf _ state [Vector a] = pretty state a "(" ")" >>= do_io putStrLn
printf _ _ [Number a] = do_io print a
printf _ _ [String a] = do_io print a
printf _ _ [Float a] = do_io print a
printf _ _ [Char a] = do_io print a
printf _ _ [Atom a] = do_io putStrLn a
printf pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "print" 1 (length x)]
        | otherwise = goLeft [typeException pos "print" x]

printStr _ _ [String a] = do_io putStrLn a
printStr pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "printStr" 1 (length x)]
        | otherwise = goLeft [typeException pos "printStr" x]

do_io f x = EitherErr (f x >> (return . Right . Atom) "@ok")

pretty state args open close = evalArgs state args >>= return . (\y -> open ++ (intercalate "," y) ++ close) . (map show)

simplify :: State -> (Expression, Expression) -> EitherErr IO [(String, Datatype)]
simplify state (Variable _ string, expression) = eval state expression >>= \datatype -> goRight [(string, datatype)]
simplify state (pattern, application) = goLeft []

eval :: State -> Expression -> EitherErr IO Datatype
--eval state (Application pos (Lambda x y) args) = lambda state pos x y args
eval state (Application pos fun args) = apply (translateFun fun) state pos fun args
eval _ (Datatype _ x) = goRight x
eval state (Variable pos str)
    | getVariable state str == Nothing = goLeft [CompileError "asdf" pos ("variable " ++ str ++ " unbound")]
    | otherwise = (goRight . fromJust) (getVariable state str)
eval a b = goRight (Atom "foo")

apply Nothing _ p fun _ = goLeft [nameException p fun]
apply (Just fun) state pos _ args = evalArgs state args >>= fun pos state

translateFun fun = lookup (value fun) functionList

pureFunctions = [("+", add), ("-", sub), ("neg", neg), ("*", mul), ("/", division), ("abs", StdLib.abs)]
impureFunctions = [("print", printf), ("printStr", printStr)]
functionList = (pureFunctions ++ impureFunctions)

evalArgs state args = mapM (eval state) args

-- internal functions

goRight :: a -> EitherErr IO a
goRight = EitherErr . return . Right
goLeft = EitherErr . return . Left

tuplef _ _ _ [] = goRight (Vector [])
tuplef pos state f argMatrix =
    mapM (\argList -> evalArgs state argList >>= f pos state) (transpose argMatrix) >>=
    goRight . Vector . (map (Datatype pos))

tupleMul _ _ _ [] _ = goRight (Vector [])
tupleMul pos state f argList c =
    evalArgs state argList >>=
    mapM (\x -> Datatype pos <$> (f pos state [x,c])) >>=
    goRight . Vector

--reportTypeError :: String -> [Datatype] -> CompileError

