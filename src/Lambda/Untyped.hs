{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped where

import Control.Monad.State
import Control.Monad.Reader
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

-- Here are some examples of lambdas in this structure:
ex :: Int -> Lambda
ex 0 = "x"
-- ^ x
ex 1 = App "x" "x"
-- ^ xx
ex 2 = Abs "x" (App "x" "x")
-- ^ \x . xx

-- | The "prefix-only" style of function application can be limiting and
-- difficult to read sometimes. We can introduce these infix operators to make
-- it a little easier to construct and read lambda expressions.
(~>) :: Variable -> Lambda -> Lambda
(~>) = Abs
infixr 8 ~>

(#) :: Lambda -> Lambda -> Lambda
(#) = App
infixl 9 #

ex' :: Int -> Lambda
ex' 0 = "x"
ex' 1 = "x" # "x"
ex' 2 = "x" ~> "x" # "x"


-- | The expression below gets converted to the self-application lambda:
-- >>> selfApp
-- Abs (Variable "x") (App (Var (Variable "x")) (Var (Variable "x")))
selfApp :: Lambda
selfApp = "x" ~> "x" # "x"


-- | A more complicated example. This one parses like you'd hope:
-- >>> yCombinator
-- Abs (Variable "y") (App (Abs (Variable "x") (App (Var (Variable "y")) (App (Var (Variable "x")) (Var (Variable "x"))))) (Abs (Variable "x") (App (Var (Variable "y")) (App (Var (Variable "x")) (Var (Variable "x"))))))
yCombinator :: Lambda
yCombinator =
    "y" ~> ("x" ~> "y" # ("x" # "x")) # ("x" ~> "y" # ("x" # "x"))


-- | We can represent numbers by a repeated application of a function to
-- a value.
zero :: Lambda
zero = "f" ~> "x" ~> "x"

one :: Lambda
one = "f" ~> "x" ~> "f" # "x"

succL :: Lambda
succL = "m" ~> "f" ~> "x" ~> "f" # ("m" # "f" # "x")


-- | Two lambda expressions are alpha equivalent if renaming the bound variables
-- in the expression can yield the same expression. We can express this
-- recursively over the lambda data type. We will need to keep track of the
-- _free_ variables in the expressions, which must match.
--
-- >>> :set -XOverloadedStrings
-- >>> "x" ~> "x" === "y" ~> "y"
-- True
-- >>> "x" ~> "y" === "z" ~> "y"
-- True
-- >>> "x" ~> "y" === "x" ~> "z"
-- False
-- >>> "x" === "x"
-- True
alphaEquivalent :: Lambda -> Lambda -> Bool
alphaEquivalent a b = evalState (alpha a b) mempty

(===) :: Lambda -> Lambda -> Bool
(===) = alphaEquivalent

infix 4 ===


-- | Alpha operates with a state of set of bound variables. Using deBruijn
-- indexing would be a neat way to not need to worry about variable names, but
-- this just keeps track of a set of variables.
alpha :: Lambda -> Lambda -> State (Set (Variable, Variable)) Bool
-- | The Abstraction case introduces variable names into the state.
alpha (Abs varA a) (Abs varB b) = do
    modify (Set.insert (varA, varB))
    alpha a b
-- | The Application case relies on the property of Compatibility in the
-- textbook (p10). If M =a N, then LM =a LN and ML =a NL.
alpha (App a b) (App c d) =
    (&&) <$> alpha a c <*> alpha b d
-- | If we're comparing two variables, then we'll need to check to see if
-- they're present in the set of bound variables. If not, then we'll compare
-- them for equality.
alpha (Var a) (Var b) =
    (|| a == b) <$> gets (Set.member (a, b))
-- | If we're at a place where the two constructors don't match up, then we are
-- not alpha equivalent, and can quit.
alpha _ _ = return False


-- | A common operation is to retrieve the free variables of a given lambda
-- expression. A free variable is a term in the expression that is not bound by
-- lambda expression.
--
-- >>> freeVariables ("x" ~> "x")
-- fromList []
-- >>> freeVariables ("x" ~> "y")
-- fromList [Variable "y"]
-- >>> freeVariables yCombinator
-- fromList []
-- >>> freeVariables ("x" ~> "y" # "z" # "x")
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


-- | Finally, we approach the problem of beta reduction. In the simple case, we
-- match on the application of an expression to a lambda abstraction. Otherwise,
-- we return the expression, as it is non-reducible.
--
-- TODO: check that variables are handled properly!!
betaReduction :: Lambda -> Lambda
betaReduction (App (Abs v body) expr) =
    substitute body (v := expr)
-- betaReduction (Var other) = _f
-- betaReduction (App other1 other2) = _f
-- betaReduction (Abs other1 other2) = _f
betaReduction o = o
