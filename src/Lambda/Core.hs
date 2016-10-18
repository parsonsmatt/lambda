module Lambda.Core where

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

newtype Fix f = Wrap { unwrap :: f (Fix f) }

type Lambda = Fix (LambdaF Variable)

var :: String -> Lambda
var = Wrap . Var . Variable

(#) :: Lambda -> Lambda -> Lambda
l # r = Wrap (App l r)

infixl 8 #

(~>) :: String -> Lambda -> Lambda
v ~> x = Wrap (Abs (Variable v) x)

infixr 9 ~>
