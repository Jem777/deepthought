module StdLib where

import Types
import Data.List


eval :: State -> Expression -> Datatype
eval a b = Atom "foo"

-- the functions

add :: Datatype -> Datatype -> Either CompileError Datatype
add (List a) (List b) = Right (List (a ++ b))
add (Tupel a) (Tupel b) = Left (CompileError "TypeError" testEmptyPos "") --tupelAdd a b
add (Number a) (Number b) = Right (Number (a + b))
add (Float a) (Number b) = Right (Float (a + (fromInteger b)))
add (Number a) (Float b) = Right (Float ((fromInteger a) + b))
add (Float a) (Float b) = Right (Float (a + b))
add (String a) (String b) = Right (String (a ++ b))
add _ _ = Left (CompileError "TypeError" testEmptyPos "asdf")

sub :: Datatype -> Datatype -> Either CompileError Datatype
sub (List a) (List b) = Right (List (a \\ b)) -- has the be evaled first
sub (Tupel a) (Tupel b) = Left (CompileError "TypeError" testEmptyPos "")
sub (Number a) (Number b) = Right (Number (a - b))
sub (Float a) (Number b) = Right (Float (a - (fromInteger b)))
sub (Number a) (Float b) = Right (Float ((fromInteger a) - b))
sub (Float a) (Float b) = Right (Float (a - b))
sub (String a) (String b) = Right (String (a \\ b))
sub _ _ = Left (CompileError "TypeError" testEmptyPos "asdf")

negete :: Datatype -> Either CompileError Datatype
negete (Number a) = Right (Number (negate a))
negete (Float a) = Right (Float (negate a))
negete _ = Left (CompileError "TypeError" testEmptyPos "asdf")

--listconstructor ()


-- internal functions

tupelAdd [] [] = Right (Tupel [])
tupelAdd a b | (length a) == (length b) = Right (Tupel (a ++ b))
             | otherwise = Left (CompileError "TypeError" testEmptyPos "length of the tupels don't match")


