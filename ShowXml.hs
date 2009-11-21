module ShowXml 
    where

import Types

class ShowXml a where
    showxml :: a -> String

instance ShowXml Expression where
    showxml (Variable str) = "<variable>\"" ++ str ++ "\"</variable>"
    showxml (Application fun args) = "<application><function>" ++ (show fun) ++"</function><arguments>" ++ concat (map show args) ++ "</arguments></application>"
    showxml (Datatype t) = "\"" ++ show t ++ "\""
    showxml Wildcard = "wildcard"

instance ShowXml Datatype where
    showxml (List expr) = "<list>" ++ concat (map showxml expr) ++ "</list>"
    showxml (Tupel expr) = "<list>" ++ concat (map showxml expr) ++ "</list>"
    showxml (Number int) = "<number>" ++ show int ++ "</number>"
    showxml (Float fl) = "<float>" ++ show fl ++ "</float>"
    showxml (String str) = "<list>" ++ concat (map showxml str) ++ "</list>"
    showxml (Char char) = "<char>"
    showxml (Atom str) = "<atom>"
    showxml (Operator op) = "<operator>"
    showxml (Lambda (p:[]) expr) = "<lambda>" ++ concat (map showxml expr ++ "</lambda>"
    showxml (Lambda (p:ps) expr) = "<lambda>" ++ concat (map show patr) ++ "</lambda>"
