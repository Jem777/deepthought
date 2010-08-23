module Eval where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Types
import Misc

class Evaluate a where
    eval :: State -> a -> EitherErr IO Datatype

class Execute a where
    exec :: a -> SourcePos -> State -> [Datatype] -> EitherErr IO Datatype

