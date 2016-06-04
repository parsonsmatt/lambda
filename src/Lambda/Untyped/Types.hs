{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Lambda.Untyped.Types where

import Control.Monad.Free
import Data.Text (Text)

-- | This representation of the lambda calculus has the base cases of the
-- recursion taken out. This lays bare the structure of the branching.
data LambdaF r
    = App r r
    | Abs Text r
    deriving (Eq, Show)

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
