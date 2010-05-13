module Test where

import Test.HUnit
import Parser
import Expr
import Types


parser = genList (combine listComplex listPrimitive)

--------------------
-- Parser testing --
--------------------

-- exception on wrong syntax?



-- right parsing


genList (f, l) = TestList (map (\(a, b, c) -> (TestCase (assertEqual a (show b) (testParse f c)))) l)

listExpression = (expression, [
        ("Expression", (Lambda [Variable "Bar"] (Datatype (Number 3))), "(\\Bar -> 3)")
        ])

combine a b = (fst a, [(x y) | x <- (snd a), y <- (snd b)])

listFunction = (function, [])

listComplex = (expression, [
        (\(n, a, b) -> ("Lambda /w " ++ n, (Lambda [Variable "Bar"] (Datatype a)), "(\\Bar -> " ++ b ++ ")")),
        (\(n, a, b) -> ("Application /w " ++ n, (Application (Datatype (Atom "bar")) [Datatype a]), "bar " ++ b)),
        (\(n, a, b) -> ("Operation (+)/w " ++ n, (Application (Datatype (Operator "+")) [(Datatype (Number 2)), (Datatype a)]), "2 + " ++ b)),
        (\(n, a, b) -> ("Operation (*)/w " ++ n, (Application (Datatype (Operator "*")) [(Datatype a), (Datatype a)]), b ++ "*" ++ b))
        ])

listPrimitive = (primitive, [
        ("Number dez", (Number 3), "03"),
        ("Number hex", (Number 255), "0xFF"),
        ("Number oct", (Number 83), "0o123"),
        ("Float", (Float 2.4), "2.4"),
        ("Float 0.x", (Float 0.4), "0.4"),
        ("Char ASCII", (Char 'f'), "'f'"),
        ("Char Unicode", (Char 'ä'), "'ä'"),
        ("String ASCII", (String "foo bar"), "\"foo bar\""),
        ("String Unicode", (String "äöüß"), "\"äöüß\""),
        ("Atom", (Atom "atom"), "@atom"),
        ("Atom /w num", (Atom "atom123"), "@atom123"),
        ("Atom /w underscore", (Atom "atom_f"), "@atom_f"),
        ("Atom /w uppercase", (Atom "atOM"), "@atOM")
        ])
