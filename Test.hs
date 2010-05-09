module Test where

import Test.HUnit
import Parser
import Expr
import Types



--------------------
-- Parser testing --
--------------------

-- exception on wrong syntax?



-- right parsing


genList (f, l) = TestList (map (\(a, b, c) -> (TestCase (assertEqual a (show b) (testParse f c)))) l)

listExpression = (expression, [
        ("Expression", (Lambda [Variable "Bar"] (Datatype (Number 3))), "(\\Bar -> 3)")
        ])
listPrimitive = (primitive, [
        ("Number dez", (Number 3), "03"),
        ("Number neg", (Number (-2)), "-2"),
        ("Number neg", (Number (-22)), "-22"),
        ("Number hex", (Number 255), "0xFF"),
        ("Number oct", (Number 83), "0o123"),
        ("Number hex neg", (Number (-255)), "-0xFF"),
        ("Number oct neg", (Number (-83)), "-0o123"),
        ("Float", (Float 2.4), "2.4"),
        ("Float 0.x", (Float 0.4), "0.4"),
        ("Float neg", (Float (-4.333)), "-4.333"),
        ("Char ASCII", (Char 'f'), "'f'"),
        ("Char Unicode", (Char 'ä'), "'ä'"),
        ("String ASCII", (String "foo bar"), "\"foo bar\""),
        ("String Unicode", (String "äöüß"), "\"äöüß\""),
        ("Atom", (Atom "atom"), "@atom"),
        ("Atom /w num", (Atom "atom123"), "@atom123"),
        ("Atom /w underscore", (Atom "atom_f"), "@atom_f"),
        ("Atom /w uppercase", (Atom "atOM"), "@atOM")
        ])
