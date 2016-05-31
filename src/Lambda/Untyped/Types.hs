module Lambda.Untyped.Types where

import Data.Text (Text)

-- | Free is a way to factor out the recursion of a data type. If our data type
-- has some terminating base case, then we can put all of that onto the `Pure`
-- constructor. The functor that describes the recursive branching is kept to
-- the `Free` constructor.
data Free f a
    = Free (f (Free f a))
    | Pure a

-- | This representation of the lambda calculus has the base cases of the
-- recursion taken out. This lays bare the structure of the branching.
data LambdaF r
    = App r r
    | Abs Text r

-- | The type of literals is how we'll terminate the recursion. We leave the
-- type of variables polymorphic.
data Literal a
    = Var a
    | Str Text
    | Int Integer
    deriving (Eq, Show)

-- | The type of our parsing, then, is taking the free monad of the lambda
-- calculus functor with the leaves of the tree denoted by the literal type.
type Parsed a = Free LambdaF (Literal a)

type Eval a = Free LambdaF (Either Int (Literal a))

instance Functor LambdaF where
    fmap k (App f x) = App (k f) (k x)
    fmap k (Abs n e) = Abs n (k e)
