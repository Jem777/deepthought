module StdLib where

-- System Imports
import Data.List
import Data.Either (lefts, rights, either)
import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative ((<$>), (<*>))

-- Deepthought Imports
import Types
import Eval
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
add pos _ x = goLeft [functionException pos "+" 2 x]

--sub :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
sub _ _ [List a, List b] = goRight (List (a \\ b)) -- has the be evaled first
sub pos state [Vector a, Vector b] = tuplef pos state sub [a,b]
sub _ _ [Number a, Number b] = goRight (Number (a - b))
sub _ _ [Float a, Number b] = goRight (Float (a - (fromInteger b)))
sub _ _ [Number a, Float b] = goRight (Float ((fromInteger a) - b))
sub _ _ [Float a, Float b] = goRight (Float (a - b))
sub _ _ [String a, String b] = goRight (String (a \\ b))
sub pos _ x = goLeft [functionException pos "-" 2 x]

neg _ _ [Number a] = goRight (Number (negate a))
neg _ _ [Float a] = (goRight . Float . negate) a
neg pos _ x = goLeft [functionException pos "negate" 1 x]

abs _ _ [Number a] | a > 0 = goRight (Number a) | otherwise = goRight (Number (negate a))
abs _ _ [Float a] | a > 0 = goRight (Float a) | otherwise = goRight (Float (negate a))
abs pos _ x = goLeft [functionException pos "abs" 1 x]

mul _ _ [List a, Number b] = goRight (List (concat (replicate (fromInteger b) a)))
mul pos state [Vector a, Number b] = tupleMul pos state mul a (Number b)
mul pos state [Number b, Vector a] = tupleMul pos state mul a (Number b)
--mul pos state [Vector a, Vector b] = G
mul _ _ [Number a, Number b] = goRight (Number (a * b))
mul _ _ [Float a, Number b] = goRight (Float (a * (fromInteger b)))
mul _ _ [Number a, Float b] = goRight (Float ((fromInteger a) * b))
mul _ _ [Float a, Float b] = goRight (Float (a * b))
mul _ _ [String a, Number b] = goRight (String (concat (replicate (fromInteger b) a)))
mul _ _ [Number b, String a] = goRight (String (concat (replicate (fromInteger b) a)))
mul pos _ x = goLeft [functionException pos "*" 2 x]

division _ _ [Number a, Number b] = goRight (Float ((fromInteger a) / (fromInteger b)))
division _ _ [Float a, Number b] = goRight (Float (a / (fromInteger b)))
division _ _ [Number a, Float b] = goRight (Float ((fromInteger a) / b))
division _ _ [Float a, Float b] = goRight (Float (a / b))
division pos _ x = goLeft [functionException pos "/" 2 x]

listconstructor pos _ [a, List b] = goRight (List ((Datatype pos a):b))
listconstructor pos _ x = goLeft [functionException pos ":" 2 x]

printf _ state [x] = prettyShow state x >>= do_io putStrLn
printf pos _ x = goLeft [functionException pos "print" 1 x]

printStr _ _ [String a] = do_io putStrLn a
printStr pos _ x = goLeft [functionException pos "printStr" 1 x]

do_io f x = EitherErr (f x >> (return . Right . Atom) "@ok")

string _ state [x] = prettyShow state x >>= goRight . String
string pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "str" 1 (length x)]
        | otherwise = goLeft [typeException pos "str" x]

prettyShow state (List args) = evalArgs state args >>= mapM (prettyShow state) >>= parens "[" "]"
prettyShow state (Vector args) = evalArgs state args >>= mapM (prettyShow state) >>= parens "(" ")"
prettyShow _ (Number a) = (goRight . show) a
prettyShow _ (Float a) = (goRight . show) a
prettyShow _ (String a) = (goRight . show) a
prettyShow _ (Char a) = (goRight . show) a
prettyShow _ (Atom a) = goRight a

parens open close str = goRight (open ++ (intercalate "," str) ++ close)

simplify :: State -> (Expression, Datatype) -> EitherErr IO [(String, Datatype)]
simplify state (Variable _ string, datatype) = goRight [(string, datatype)]
simplify state (Datatype _ (List a), List b) = merge state a b
simplify state (Datatype _ (Vector a), Vector b) = merge state a b
simplify state (Datatype _ datatype, datatype2) = ifElse (datatype == datatype2) (goRight []) (goLeft [])
simplify state (pattern, application) = goLeft []
merge state a b = evalArgs state b >>= \b1 -> concat <$> mapM (simplify state) (zip a b1)

setState f state args pattern = (f state) . concat <$> mapM (simplify state) (zip pattern args)
addToState = setState addVariables

checkGuard _ Wildcard = goRight ()
checkGuard state expression = eval state expression >>= \x -> ifElse (x == Atom "@true") (goRight ()) (goLeft [patternException expression])

evalGuard f state pattern guard body args = (setState f) state args pattern >>= \state1 -> checkGuard state1 guard >> return (state1, body)

instance Evaluate Expression where
    eval state (Application pos fun args) = evalArgs state args >>= exec fun pos state
    eval _ (Datatype _ x) = goRight x
    eval state (Variable pos str)
        | getVariable state str == Nothing = goLeft [varUnbound (Variable pos str)]
        | otherwise = (goRight . fromJust) (getVariable state str)

instance Evaluate Datatype where
    eval _ a = goRight a

instance Execute Datatype where
    exec (Lambda pattern body) pos state args = addToState state args pattern >>= \s -> eval s body

instance Execute Expression where
    exec (Datatype _ x) p s a = exec x p s a
    exec (Operator pos str) p state args = maybe (goLeft []) (\fun -> evalArgs state args >>= fun p state) (lookup str functionList)

instance Execute Definition where
    exec (Definition p name list) pos state args = msum (map f list) >>= uncurry eval
        where f (pattern, guard, body, inlineFunc) = evalGuard addVariables state pattern guard body args --ToDo: add support for inline functions
    exec (InlineFunction p name list) pos state args = msum (map f list) >>= uncurry eval
        where f (pattern, guard, body) = evalGuard addVariables state pattern guard body args


pureFunctions = [
    ("+", add),
    ("-", sub),
    ("neg", neg),
    ("*", mul),
    ("/", division),
    ("abs", StdLib.abs),
    ("str", string),
    (":", listconstructor)
    ]
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

