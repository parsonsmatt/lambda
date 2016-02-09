module Lambda.Common where

import Data.String

-- | Variable is a newtype wrapper around a String which carries a phantom type
-- variable. This allows Term and Type variables to be safely separated.
newtype Variable t
    = Variable String
    deriving (Eq, Ord, Show)

instance IsString (Variable a) where
    fromString = Variable
