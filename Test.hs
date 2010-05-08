module Test where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Parser
import Expr
import Types

--------------------
-- Parser testing --
--------------------

-- exception on wrong syntax?



-- right parsing

test1_expression = TestCase (assertEqual "Expression" (show (Lambda [Variable "Bar"] (Datatype (Number 3)))) (testParse expression "(\\Bar -> 3)"))


test_list (f, l) = TestList (map (\(a, b, c) -> (TestCase (assertEqual a (show b) (testParse f c)))) l)

test1_list = [
        ("Expression", (Lambda [Variable "Bar"] (Datatype (Number 3))), "(\\Bar -> 3)")
        ]
test1_primitive = (primitive, [
        ("Number dez", (Number 3), "03"),
        ("Number neg", (Number (-2)), "-2"),
        ("Number neg", (Number (-22)), "-22"),
        ("Number hex", (Number 255), "0xFF"),
        ("Number oct", (Number 83), "0o123"),
        ("Float", (Float 2.4), "2.4"),
        ("Float neg", (Float (-4.333)), "-4.333")
        ])
