module Misc where

import Data.Either (either, lefts, rights)

eitherRight f = either Left (Right . f) 

mapEither f (Right x) (Right y) = Right (f x y)
mapEither _ (Left x) (Left y) = Left (x ++ y)
mapEither _ (Left x) _ = Left x
mapEither _ _ (Left x) = Left x

eitherFold _ _ [] = Right []
eitherFold f g l
    | (not . null) (lefts l) = Left (foldl1 f (lefts l))
    | otherwise = Right (foldl1 g (rights l))
