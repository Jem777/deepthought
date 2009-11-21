module ShowXml 
    where

import Types

class ShowXml a where
    showxml :: a -> String

instance ShowXml Expression where
    showxml (Variable str) = "<variable>" ++ str ++ "</variable>"
    showxml (Application fun args) = "<application><function>" ++ (showxml fun) ++"</function><arguments>" ++ concat (map showxml args) ++ "</arguments></application>"
    showxml (Datatype t) = showxml t 
    showxml Wildcard = "wildcard"

instance ShowXml Datatype where
    showxml (List expr) = "<list>" ++ concat (map showxml expr) ++ "</list>"
    showxml (Tupel expr) = "<list>" ++ concat (map showxml expr) ++ "</list>"
    showxml (Number int) = "<number>" ++ show int ++ "</number>"
    showxml (Float fl) = "<float>" ++ show fl ++ "</float>"
    showxml (String str) = "<list>" ++ concat (map (showxml . Char) str) ++ "</list>"
    showxml (Char char) = "<char>" ++ char:[] ++ "</char>"
    showxml (Atom str) = "<atom>" ++ str ++ "</atom>"
    showxml (Operator op) = "<operator>" ++ op ++ "</operator>"
    showxml (Lambda (p:[]) expr) = "<lambda><pattern>" ++ (showxml p) ++ "</pattern><expression>" ++ (showxml expr) ++ "</expression></lambda>"
    showxml (Lambda (p:ps) expr) = "<lambda><pattern>" ++ (showxml p) ++ "</pattern><expression>" ++ showxml (Lambda ps expr) ++ "</expression></lambda>"
