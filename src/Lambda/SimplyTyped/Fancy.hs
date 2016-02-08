{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lambda.SimplyTyped.Fancy where

import Data.String

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
    = TyVar Nat
    | TyArr Type Type
    deriving (Show, Eq)

data Nat = Z | S Nat
    deriving (Show, Eq, Read)

instance IsString Type where
    fromString = TyVar . read

type a :-> b = 'TyArr a b

type LUnit = TyVar Z
type LBool = TyVar (S Z)

-- | Variables now contain a phantom type which indicates whether they are Type
-- variables or Term variables.
newtype Variable t = Variable String
    deriving (Show, Eq)

instance IsString (Variable t) where
    fromString = Variable

-- | Since we're dealing with types now, we have to be more clever with our
-- representation of terms. The Haskell data type 'Term' takes a lifted data of
-- kind Type and yields a value of kind *.
--
-- The constructor Var takes a Variable indexed on the type variable, and
-- yields a Term indexed by the type given in the variable.
--
-- Application takes a term that has a type of `a :-> b` as the first thing,
-- a `Term a` as the second thing, and returns a `Term b` as the final value.
--
-- Abstraction takes a Variable with type a, a Term of type b, and returns
-- a Term of type `a -> b`.
--
-- While somewhat complex, this allows us to use Haskell's compiler to ensure
-- that we don't construct invalid terms.
data Term :: Type -> * where
    Var :: Variable (Term a) -> Term a
    App :: Term (a :-> b) -> Term a -> Term b
    Abs :: Variable (Term a) -> Term b -> Term (a :-> b)

deriving instance Show (Term n)

instance IsString (Term a) where
    fromString = Var . Variable

-- | The IsString instance makes referring to terms easy. We do need to annotate
-- them with a type, or they'll be 'Term a' where 'a' is any type. We don't have
-- type polymorphism yet, so we can't do that!
x :: Term LUnit
x = "x"

y :: Term LBool
y = Var (Variable "y")

abs' :: Term (LUnit :-> LBool)
abs' = Abs (Variable "x") (Var (Variable "y"))

-- | 'app'' won't type check if we apply terms with incompatible values. 'App
-- x abs'' causes a compile time type error. In this way, we've punted type
-- checking to GHC itself!
app' :: Term LBool
app' = App abs' x

-- As fun as all of this is, punting type checking to Haskell isn't the best way
-- to actually learn about how the simply typed lambda calculus works.
