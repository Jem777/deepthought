module Misc where

import Data.Either (either, lefts, rights)

eitherRight f = either Left (Right . f) 
