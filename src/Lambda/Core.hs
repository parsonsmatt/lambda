module Lambda.Core where

import Data.Functor.Foldable

-- | The data type LambdaF is parameterized over the type of variables and the
-- type of
data LambdaF v x
    = Var v
    | App x x
    | Abs v x
    deriving (Eq, Show)

newtype Variable = Variable String
    deriving (Eq, Show)

data Untyped

type Lambda = Fix (LambdaF Variable)

var :: String -> Lambda
var = Fix . Var . Variable

(#) :: Lambda -> Lambda -> Lambda
l # r = Fix (App l r)

infixl 8 #

(~>) :: String -> Lambda -> Lambda
v ~> x = Fix (Abs (Variable v) x)

infixr 9 ~>
