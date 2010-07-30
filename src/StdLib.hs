module StdLib where

-- System Imports
import Data.List
import Data.Either (lefts, rights, either)
import Data.Maybe (fromJust)
import Control.Monad (ap, mapM, foldM)
import Control.Applicative ((<$>), (<*>))

-- Deepthought Imports
import Types
import Errors
import Misc

-- the functions



--add :: SourcePos -> Datatype -> Datatype -> Either [CompileError] Datatype
--add _ [x] = goRight (Lambda foo bar)
add _ _ [List a, List b] = goRight (List (a ++ b))
--add pos state [Tupel a, Tupel b] = tuplef pos state add a b --goLeft [CompileError "TypeError" testEmptyPos ""] --tupelAdd a b
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
sub _ _ [Tupel a, Tupel b] = goLeft [CompileError "TypeError" testEmptyPos ""]
sub _ _ [Number a, Number b] = goRight (Number (a - b))
sub _ _ [Float a, Number b] = goRight (Float (a - (fromInteger b)))
sub _ _ [Number a, Float b] = goRight (Float ((fromInteger a) - b))
sub _ _ [Float a, Float b] = goRight (Float (a - b))
sub _ _ [String a, String b] = goRight (String (a \\ b))
sub pos _ x
        | (length x) > 2 = goLeft [tooMuchArguments pos "-" 2 (length x)]
        | otherwise = goLeft [typeException pos "-" x]

neg :: SourcePos -> State -> [Datatype] -> IO (Either [CompileError] Datatype)
neg _ _ [Number a] = goRight (Number (negate a))
neg _ _ [Float a] = goRight (Float (negate a))
neg pos _ x
        | (length x) > 1 = goLeft [tooMuchArguments pos "negate" 1 (length x)]
        | otherwise = goLeft [typeException pos "negate" x]

mul _ _ [List a, Number b] = goRight (List (concat (replicate (fromInteger b) a)))
mul _ _ [Tupel a, Number b] = goLeft [CompileError "TypeError" testEmptyPos ""] -- has to be evaled first
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

printf pos state [List a] = eitherFold (++)
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

do_io f x = f x >> goRight (Atom "@ok")



simplify :: State -> (Expression, Expression) -> IO (Either [CompileError] [(String, Datatype)])
simplify state (Variable _ string, expression) =
    (eval state expression) >>= 
    (return . (either Left (\datatype -> Right [(string, datatype)])))
simplify state (pattern, application) = goLeft []

--eval state (Application pos (Lambda x y) args) = lambda state pos x y args
eval state (Application pos fun args) = apply (translateFun fun) state pos fun args
eval _ (Datatype _ x) = goRight x
eval state (Variable pos str)
    | getVariable state str == Nothing = goLeft [CompileError "asdf" pos ("variable " ++ str ++ " unbound")]
    | otherwise = (goRight . fromJust) (getVariable state str)
eval a b = goRight (Atom "foo")

apply Nothing _ p fun _ = goLeft [nameException p fun]
apply (Just f) state pos fun args = (apply2 pos) state f args

apply2 pos state fun args = (mapM evaluate args) >>= help (fun pos state)
    where
    evaluate x = (return x) >>= (eval state)
    help f l
        | (lefts l == []) = f (rights l)
        | otherwise = goLeft (concat (lefts l))

translateFun fun = lookup (value fun) functionList

pureFunctions = [("+", add), ("-", sub), ("neg", neg), ("*", mul), ("/", division)]
impureFunctions = [("print", printf), ("printStr", printStr)]
functionList = (pureFunctions ++ impureFunctions)

-- internal functions

goRight = return . Right
goLeft = return . Left

--tuplef _ _ _ [] = goRight (Tupel [])
--tuplef pos state f argMatrix = foldM help (Right (Tupel [])) (map (\argList -> mapM (eval state) argList >>= (eFold (f pos state))) (transpose argMatrix))
--    where

--help :: (Monad m) => Either [a] Datatype -> m (Either [a] Datatype) -> m (Either [a] Datatype)
--help x y = y >>= (mapEither (\a b -> Tupel ((Datatype a):b)) x)

--eitherFold _ [] = Right []
eFold f l
    | (not . null) (lefts l) = goLeft (foldl1 (++) (lefts l))
    | otherwise = (f (rights l))


--reportTypeError :: String -> [Datatype] -> CompileError

