module Lambda.Common where

import Data.String

newtype Variable
    = Variable String
    deriving (Eq, Ord, Show)

instance IsString Variable where
    fromString = Variable
