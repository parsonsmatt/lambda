{-# LANGUAGE OverloadedStrings #-}

module Lambda.SimplyTyped where

import Data.String
import Lambda.Common

-- | The simply typed lambda calculus begins with the untyped lambda calculus
-- and adds types. Types have the following construction:
--
-- 1. A type variable, or
-- 2. An arrow type

-- We represent these below. Rather than keeping a string or other identifier,
-- we'll use the natural numbers to index types. This design decision is
-- unfortunately caused by a limitation of Haskell's type system around what
-- sorts of values can be lifted into the type level.
data Type
    = TyVar (Variable Type)
    | TyArr Type Type
    deriving (Show, Eq)

instance IsString Type where
    fromString = TyVar . Variable

-- | Now that we've defined types, we need to define a means of assigning a type
-- to a lambda term.
data Term
    = Term :. Type
    | Var (Variable Term)
    | App Term Term
    | Abs (Variable Term) Term
    deriving (Eq, Show)
