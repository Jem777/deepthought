module StdLib where

import AST
import ASTErrors
import Misc

import Data.List (transpose, (\\))
import Control.Monad (zipWithM, foldM)

add :: SourcePos -> [ASTDatatype] -> Eval ASTDatatype
add _ [List a, List b] = goRight (List (a ++ b))
add pos [Vector a, Vector b]
    | (length a == length b) = mapM (add pos) (transpose [a,b]) >>= return . Vector
    | otherwise = goLeft []
add _ [Number a, Number b] = goRight (Number (a + b))
add _ [Float a, Number b] = goRight (Float (a + (fromInteger b)))
add _ [Number a, Float b] = goRight (Float ((fromInteger a) + b))
add _ [Float a, Float b] = goRight (Float (a + b))
add _ [String a, String b] = goRight (String (a ++ b))
add pos x = goLeft [functionError pos "+" 2 x]

sub _ [List a, List b] = goRight (List (a \\ b))
sub pos [Vector a, Vector b]
    | (length a == length b) = mapM (sub pos) (transpose [a,b]) >>= goRight . Vector
    | otherwise = goLeft []
sub _ [Number a, Number b] = goRight (Number (a - b))
sub _ [Float a, Number b] = goRight (Float (a - (fromInteger b)))
sub _ [Number a, Float b] = goRight (Float ((fromInteger a) - b))
sub _ [Float a, Float b] = goRight (Float (a - b))
sub _ [String a, String b] = goRight (String (a \\ b))
sub pos x = goLeft [functionError pos "-" 2 x]

neg _ [Number a] = goRight (Number (negate a))
neg _ [Float a] = (goRight . Float . negate) a
neg pos x = goLeft [functionError pos "negate" 1 x]

abs _ [Number a] = (goRight . Number . Prelude.abs) a
abs _ [Float a] = (goRight . Float . Prelude.abs) a
abs pos x = goLeft [functionError pos "abs" 1 x]

mul _ [List a, Number b] = goRight (List (concat (replicate (fromInteger b) a)))
mul pos [Vector a, Number b] = mapM (\x -> mul pos [x,Number b]) a >>= goRight . Vector
mul pos [Number a, Vector b] = mapM (\x -> mul pos [x,Number a]) b >>= goRight . Vector
mul pos [Vector [], Vector []] = (goRight . Vector) []
mul pos [Vector a, Vector b]
    | (length a) /= (length b) = goLeft []
    | otherwise = zipWithM (\x y -> mul pos [x,y]) a b >>= \(z:zs) -> foldM (\x y -> add pos [x,y]) z zs
mul _ [Number a, Number b] = goRight (Number (a * b))
mul _ [Float a, Number b] = goRight (Float (a * (fromInteger b)))
mul _ [Number a, Float b] = goRight (Float ((fromInteger a) * b))
mul _ [Float a, Float b] = goRight (Float (a * b))
mul _ [String a, Number b] = goRight (String (concat (replicate (fromInteger b) a)))
mul _ [Number b, String a] = goRight (String (concat (replicate (fromInteger b) a)))
mul pos x = goLeft [functionError pos "*" 2 x]

div _ [Number a, Number b] = goRight (Float ((fromInteger a) / (fromInteger b)))
div _ [Float a, Number b] = goRight (Float (a / (fromInteger b)))
div _ [Number a, Float b] = goRight (Float ((fromInteger a) / b))
div _ [Float a, Float b] = goRight (Float (a / b))
div pos x = goLeft [functionError pos "/" 2 x]

listconstructor pos [a, List b] = goRight (List (a:b))
listconstructor pos x = goLeft [functionError pos ":" 2 x]

printf _ [x] = (doIO putStrLn . show) x
printf pos x = goLeft [functionError pos "print" 1 x]

printStr _ [String a] = doIO putStrLn a
printStr pos x = goLeft [functionError pos "printStr" 1 x]

string _ [x] = (goRight . String . show) x
string pos x = goLeft [functionError pos "str" 1 x]

builtins =
    [
    ("+", add),
    ("-", sub),
    ("neg", neg),
    ("*", mul),
    ("/", StdLib.div),
    ("abs", StdLib.abs),
    ("str", string),
    (":", listconstructor),
    ("print", printf),
    ("printStr", printStr)
    ]
