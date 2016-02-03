{-# LANGUAGE GADTs #-}

module Lambda.SimplyTyped where

import Lambda.Common

-- | The simply typed lambda calculus begins with the untyped lambda calculus
-- and adds types. Types have the following construction:
--
-- 1. A type variable, or
-- 2. An arrow type

-- We represent these as follows:
data Type
    = TyVar Variable
    | Type :-> Type
