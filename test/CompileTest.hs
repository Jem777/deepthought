module CompileTest where

import Test.HUnit
import Types
import Compile

------------------------------
-- Test Suite fo Compile.hs --
------------------------------
--
--  TODOs:
-- checkVars with inlineFunction and failing leftSide

compileTests = TestList [genTest simple_vars, genTest complex_vars, genTest simple_funcs]

genTest (n, f, l) = TestLabel n (TestList (map (\(a, b, c) -> (TestCase (assertEqual a b (f c)))) l))

simple_vars = ("simple variable check", checkVars, [
    (
        "f X -> X;", 
        Right [],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Variable testEmptyPos "X") []),
    (
        "add A B -> A + B;", 
        Right [],
        Function testEmptyPos (Operator testEmptyPos "add") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "A",Variable testEmptyPos "B"]) []),
    (
        "const A B -> A;", 
        Right [Variable testEmptyPos "B"],
        Function testEmptyPos (Operator testEmptyPos "const") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Variable testEmptyPos "A") []),
    (
        "map F (X:Xs) -> (F X) : (map F Xs);",
        Right [],
        Function testEmptyPos (Operator testEmptyPos "map") [Variable testEmptyPos "F",Application testEmptyPos (Operator testEmptyPos ":") [Variable testEmptyPos "X",Variable testEmptyPos "Xs"]] Wildcard (Application testEmptyPos (Operator testEmptyPos ":") [Application testEmptyPos (Variable testEmptyPos "F") [Variable testEmptyPos "X"],Application testEmptyPos (Operator testEmptyPos "map") [Variable testEmptyPos "F",Variable testEmptyPos "Xs"]]) []),
    (
        "f A A -> A;", 
        Right [],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "A",Variable testEmptyPos "A"] Wildcard (Variable testEmptyPos "A") []),
    (
        "fail A -> A + B;", 
        Left [CompileError "Variable unbound" testEmptyPos ""],
        Function testEmptyPos (Operator testEmptyPos "fail") [Variable testEmptyPos "A"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "A",Variable testEmptyPos "B"]) [])
    ])

complex_vars = ("complex variable checks", checkVars, [
    (
        "f X -> X + y where y -> X + 2;",
        Right [],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Operator testEmptyPos "y"]) [Function testEmptyPos (Operator testEmptyPos "y") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) []]),
    (
        "f X -> X + y where y W -> X + 2;",
        Right [Variable testEmptyPos "W"],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Operator testEmptyPos "y"]) [Function testEmptyPos (Operator testEmptyPos "y") [Variable testEmptyPos "W"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) []]),
    (
        "f X -> X + y where y W -> W + 2;",
        Right [],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Operator testEmptyPos "y"]) [Function testEmptyPos (Operator testEmptyPos "y") [Variable testEmptyPos "W"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "W",Datatype testEmptyPos (Number 2)]) []]),
    (
        "fail X -> X + y where y -> W + 2;",
        Left [CompileError "Variable unbound" testEmptyPos ""],
        Function testEmptyPos (Operator testEmptyPos "fail") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Operator testEmptyPos "y"]) [Function testEmptyPos (Operator testEmptyPos "y") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "W",Datatype testEmptyPos (Number 2)]) []]),
    (
        "f X Y -> a + b where (a -> X * 2; b -> Y *3;)",
        Right [],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X",Variable testEmptyPos "Y"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Operator testEmptyPos "a",Operator testEmptyPos "b"]) [Function testEmptyPos (Operator testEmptyPos "a") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "*") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) [],Function testEmptyPos (Operator testEmptyPos "b") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "*") [Variable testEmptyPos "Y",Datatype testEmptyPos (Number 3)]) []]),
    (
        "F . G -> (\\Y -> F (G Y));",
        Right [],
        Function testEmptyPos (Operator testEmptyPos ".") [Variable testEmptyPos "F",Variable testEmptyPos "G"] Wildcard (Datatype testEmptyPos (Lambda [Variable testEmptyPos "Y"] (Application testEmptyPos (Variable testEmptyPos "F") [Application testEmptyPos (Variable testEmptyPos "G") [Variable testEmptyPos "Y"]]))) []),
    (
        "F . G -> (\\Y,X -> F (G Y));",
        Right [Variable testEmptyPos "X"],
        Function testEmptyPos (Operator testEmptyPos ".") [Variable testEmptyPos "F",Variable testEmptyPos "G"] Wildcard (Datatype testEmptyPos (Lambda [Variable testEmptyPos "Y", Variable testEmptyPos "X"] (Application testEmptyPos (Variable testEmptyPos "F") [Application testEmptyPos (Variable testEmptyPos "G") [Variable testEmptyPos "Y"]]))) []),
    (
        "fail X Y -> a + b where (a -> X * 2; b Y -> Y *3;)",
        Left [CompileError "Conflicting Definitions" testEmptyPos ""],
        Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X",Variable testEmptyPos "Y"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Operator testEmptyPos "a",Operator testEmptyPos "b"]) [Function testEmptyPos (Operator testEmptyPos "a") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "*") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) [],Function testEmptyPos (Operator testEmptyPos "b") [Variable testEmptyPos "Y"] Wildcard (Application testEmptyPos (Operator testEmptyPos "*") [Variable testEmptyPos "Y",Datatype testEmptyPos (Number 3)]) []])
    ])

simple_funcs = ("simple functions check", checkFuncs, [
    (
        "f X -> X;",
        Right [Operator testEmptyPos "f"],
        [Function testEmptyPos (Operator testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Variable testEmptyPos "X") []]),
    (
        "add A B -> A; alias A B -> add A B;",
        Right [Operator testEmptyPos "alias"],
        [Function testEmptyPos (Operator testEmptyPos "add") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Variable testEmptyPos "A") [],
        Function testEmptyPos (Operator testEmptyPos "alias") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Application testEmptyPos (Operator testEmptyPos "add") [Variable testEmptyPos "A",Variable testEmptyPos "B"]) []])
    ])

----------------------------------
-- Tests for internal functions --
----------------------------------


