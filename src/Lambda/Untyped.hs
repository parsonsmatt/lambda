{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped where

import Control.Monad.State
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

import Lambda.Common

-- | First, we construct the abstract grammar of untyped lambda terms. The set
-- Lambda consists of variables, applications of lambda expressions, and
-- abstractions of a variable over an expression.
data Lambda
    = Var Variable
    | App Lambda Lambda
    | Abs Variable Lambda
    deriving (Show, Eq)

-- Because `Var (Variable "x")` is tiresome, we'll use a shortcut:
instance IsString Lambda where
    fromString = Var . Variable

x :: Lambda
x = "x"

-- Here are some examples of lambdas in this structure:
ex :: Int -> Lambda
ex 0 = "x"
-- ^ x
ex 1 = App x x
-- ^ xx
ex 2 = Abs "x" (App x x)
-- ^ \x . xx


-- | Now, we introduce Substitutions. A Substitution is an assignment of some
-- variable to a lambda expression.
data Substitution = Variable := Lambda
    deriving (Show, Eq)


-- | Lambda substitution has three cases, corresponding with the constructors in
-- the data type. For Variables, we substitute the variable with the body of the
-- lambda expression if the variable name is equal to the variable.
substitute :: Lambda -> Substitution -> Lambda
substitute (Var s) (v := body)
    | s == v    = body
    | otherwise = Var s
-- | Substitution is an easy case. We recurse into both expressions,
-- substituting instances of the variable in each with the body of the
-- expression as needed.
substitute (App lam1 lam2) subst =
    App (substitute lam1 subst) (substitute lam2 subst)
-- | Substitution into a lambda abstraction is somewhat trickier. There is
-- a possibility that the lambda bound variable has the same string identifier
-- as the variable we are substituting for. If this is the case, then we halt
-- substituting, as bound variables have higher precedence than free variables.
substitute abs@(Abs bound expr) sub@(v := _)
    | bound == v = abs
    | otherwise  = Abs bound (substitute expr sub)

-- | The "prefix-only" style of function application can be limiting and
-- difficult to read sometimes. We can introduce these infix operators to make
-- it a little easier to construct and read lambda expressions.
(~>) :: Variable -> Lambda -> Lambda
(~>) = Abs
infixr 8 ~>

(.$) :: Lambda -> Lambda -> Lambda
(.$) = App
infixl 9 .$

-- | The expression below gets converted to the self-application lambda:
-- >>> lol
-- Abs (Variable "x") (App (Var (Variable "x")) (Var (Variable "x")))
lol :: Lambda
lol = "x" ~> "x" .$ "x"

-- | A more complicated example. This one parses like you'd hope:
-- >>> yCombinator
-- Abs (Variable "y") (App (Abs (Variable "x") (App (Var (Variable "y")) (App (Var (Variable "x")) (Var (Variable "x"))))) (Abs (Variable "x") (App (Var (Variable "y")) (App (Var (Variable "x")) (Var (Variable "x"))))))
yCombinator :: Lambda
yCombinator =
    "y" ~> ("x" ~> "y" .$ ("x" .$ "x")) .$ ("x" ~> "y" .$ ("x" .$ "x"))

-- | We can represent numbers by a repeated application of a function to
-- a value.
zero :: Lambda
zero = "f" ~> "x" ~> "x"

one :: Lambda
one = "f" ~> "x" ~> "f" .$ "x"

succL :: Lambda
succL = "m" ~> "f" ~> "x" ~> "f" .$ ("m" .$ "f" .$ "x")

-- | Finally, we approach the problem of beta reduction. In the simple case, we
-- match on the application of an expression to a lambda abstraction. Otherwise,
-- we return the expression, as it is non-reducible.
--
-- TODO: check that variables are handled properly!!
betaReduction :: Lambda -> Lambda
betaReduction (App (Abs v body) expr) =
    substitute body (v := expr)
betaReduction other = other

-- | A common operation is to retrieve the free variables of a given lambda
-- expression. A free variable is a term in the expression that is not bound by
-- lambda expression.
--
-- >>> :set -XOverloadedStrings
-- >>> freeVariables ("x" ~> "x")
-- fromList []
-- >>> freeVariables ("x" ~> "y")
-- fromList [Variable "y"]
-- >>> freeVariables yCombinator
-- fromList []
-- >>> freeVariables ("x" ~> "y" .$ "z" .$ "x")
-- fromList [Variable "y",Variable "z"]
freeVariables :: Lambda -> Set Variable
freeVariables expr = evalState (go expr) Set.empty
  where
    go :: Lambda -> State (Set Variable) (Set Variable)
    go (Var v) = do
        isBoundVar <- gets (Set.member v)
        return $ if isBoundVar
           then Set.empty
           else Set.singleton v
    go (Abs v body) = do
        modify (Set.insert v)
        go body
    go (App a b) = do
        as <- go a
        bs <- go b
        return (as <> bs)

