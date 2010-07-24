module ParserTest where

import Test.HUnit
import Parser
import Expr
import Types
import Data.Either
import Text.ParserCombinators.Parsec


parserTests = TestList [genList (combine listComplex listPrimitive)]

--------------------
-- Parser testing --
--------------------

-- exception on wrong syntax?



-- right parsing


genList (f, l) = TestList (map (\(a, b, c) -> (TestCase (assertEqual a (Right b) (testParse f c)))) l)

listExpression = (expression, [
        ("Expression", (Datatype testEmptyPos (Lambda [Variable testEmptyPos "Bar"] (Datatype testEmptyPos (Number 3)))), "(\\Bar -> 3)")
        ])

combine a b = (fst a, [(x y) | x <- (snd a), y <- (snd b)])

listFunction = (function, [])

listComplex = (expression, [
        (\(n, a, b) -> ("Lambda /w " ++ n, (Datatype testEmptyPos (Lambda [Variable testEmptyPos "Bar"] (Datatype testEmptyPos a))), "(\\Bar -> " ++ b ++ ")")),
        (\(n, a, b) -> ("Application /w " ++ n, (Application testEmptyPos (Fun testEmptyPos "bar") [Datatype testEmptyPos a]), "bar " ++ b)),
        (\(n, a, b) -> ("Operation (+)/w " ++ n, (Application testEmptyPos (Operator testEmptyPos "+") [(Datatype testEmptyPos (Number 2)), (Datatype testEmptyPos a)]), "2 + " ++ b)),
        (\(n, a, b) -> ("Operation (*)/w " ++ n, (Application testEmptyPos (Operator testEmptyPos "*") [(Datatype testEmptyPos a), (Datatype testEmptyPos a)]), b ++ "*" ++ b))
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

-- internal functions

testParse x y = f (parse x "" y)
    where
    f (Left a) = Left (formatError a)
    f (Right x) = (Right x)

