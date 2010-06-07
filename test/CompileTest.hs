module CompileTest where

import Test.HUnit
import Types
import Compile

compileTest = TestList [genTest simple_vars, genTest complex_vars]

genTest (n, f, l) = TestLabel n (TestList (map (\(a, b, c) -> (TestCase (assertEqual a b (f c)))) l))

simple_vars = ("simple variable check", checkVars, [
    (
        "f X -> X;", 
        Right [],
        Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Variable testEmptyPos "X") []),
    (
        "add A B -> A + B;", 
        Right [],
        Function testEmptyPos (Fun testEmptyPos "add") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "A",Variable testEmptyPos "B"]) []),
    (
        "const A B -> A;", 
        Right [Variable testEmptyPos "B"],
        Function testEmptyPos (Fun testEmptyPos "const") [Variable testEmptyPos "A",Variable testEmptyPos "B"] Wildcard (Variable testEmptyPos "A") []),
    (
        "map F (X:Xs) -> (F X) : (map F Xs);",
        Right [],
        Function testEmptyPos (Fun testEmptyPos "map") [Variable testEmptyPos "F",Application testEmptyPos (Operator testEmptyPos ":") [Variable testEmptyPos "X",Variable testEmptyPos "Xs"]] Wildcard (Application testEmptyPos (Operator testEmptyPos ":") [Application testEmptyPos (Variable testEmptyPos "F") [Variable testEmptyPos "X"],Application testEmptyPos (Fun testEmptyPos "map") [Variable testEmptyPos "F",Variable testEmptyPos "Xs"]]) []),
    (
        "fail A -> A + B;", 
        Left (CompileError "Variable unbound" testEmptyPos ""),
        Function testEmptyPos (Fun testEmptyPos "fail") [Variable testEmptyPos "A"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "A",Variable testEmptyPos "B"]) []),
    (
        "fail A A -> A;", 
        Left (CompileError "Conflicting Definitions" testEmptyPos ""),
        Function testEmptyPos (Fun testEmptyPos "fail") [Variable testEmptyPos "A",Variable testEmptyPos "A"] Wildcard (Variable testEmptyPos "A") [])
    ])

complex_vars = ("complex variable checks", checkVars, [
    (
        "f X -> X + y where y -> X + 2;",
        Right [],
        Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Fun testEmptyPos "y"]) [Function testEmptyPos (Fun testEmptyPos "y") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) []]),
    (
        "f X -> X + y where y W -> X + 2;",
        Right [Variable testEmptyPos "W"],
        Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Fun testEmptyPos "y"]) [Function testEmptyPos (Fun testEmptyPos "y") [Variable testEmptyPos "W"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Datatype testEmptyPos (Number 2)]) []]),
    (
        "f X -> X + y where y W -> W + 2;",
        Right [],
        Function testEmptyPos (Fun testEmptyPos "f") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Fun testEmptyPos "y"]) [Function testEmptyPos (Fun testEmptyPos "y") [Variable testEmptyPos "W"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "W",Datatype testEmptyPos (Number 2)]) []]),
    (
        "fail X -> X + y where y -> W + 2;",
        Left (CompileError "Variable unbound" testEmptyPos ""),
        Function testEmptyPos (Fun testEmptyPos "fail") [Variable testEmptyPos "X"] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "X",Fun testEmptyPos "y"]) [Function testEmptyPos (Fun testEmptyPos "y") [] Wildcard (Application testEmptyPos (Operator testEmptyPos "+") [Variable testEmptyPos "W",Datatype testEmptyPos (Number 2)]) []])
    ])

----------------------------------
-- Tests for internal functions --
----------------------------------


