module Misc where

--import Data.Either (either, lefts, rights)
import qualified Text.ParserCombinators.Parsec.Pos as P (SourcePos, newPos)


type SourcePos = P.SourcePos

moduleSep = "::"
testEmptyPos = P.newPos "" 0 0

{-
eitherRight f = either Left (Right . f) 

mapEither f (Right x) (Right y) = Right (f x y)
mapEither _ (Left x) (Left y) = Left (x ++ y)
mapEither _ (Left x) _ = Left x
mapEither _ _ (Left x) = Left x

eitherFold _ _ [] = Right []
eitherFold f g l
    | (not . null) (lefts l) = Left (foldl1 f (lefts l))
    | otherwise = Right (foldl1 g (rights l))

ifElse True a _ = a
ifElse False _ a = a

maybeIf True a = Just a
maybeIf False _ = Nothing

maybeElse (Just a) _ = a
maybeElse Nothing a = a

bindM2 f a b = a >>= \x -> b >>= f x
-}
