{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Lambda.Untyped.Eval.Free where

import Prelude hiding (abs)
import Control.Arrow ((***))
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Except
import Text.Show.Functions ()
import Control.Monad.Trans ()
-- import qualified Data.Text as T
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Lambda.Untyped.Parser.Free as Parser

import Lambda.Untyped.Types as Types

type Lambda = Eval Text

absVar :: a -> Free f (Either a b)
absVar = Pure . Left

freeVar :: a1 -> Free f (Either a (Literal a1))
freeVar = Pure . Right . Var

int :: Integer -> Either a (Free f (Literal a1))
int = fmap Right Types.int

str :: Text -> Either a (Free f (Literal a1))
str = fmap Right Types.str

pattern (:~>) :: Text -> Free LambdaF a -> Free LambdaF a
pattern x :~> m <- Free (Abs x m)
pattern (:$:) :: Free LambdaF a -> Free LambdaF a -> Free LambdaF a
pattern f :$: x <- Free (App f x)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>>


-- | Converts a parsed Lambda expression to an expression in de Bruijn notation.
--
-- >>> convert (var "x")
-- Pure (Right (Var "x"))
-- >>> convert (Types.abs "x" (var "x"))
-- Free (Abs "x" (Pure (Left 0)))
-- >>> convert (Types.abs "x" (app (var "x") (var "x")))
-- Free (Abs "x" (Free (App (Pure (Left 0)) (Pure (Left 0)))))
convert :: Parser.Lambda -> Lambda
convert = flip runReader (0, []) . convertWith

type ConvertEnv = (Int, [Text])

-- | Given an environment consisting of the current binding depth and a list of
-- bound variables, converts a parsed lambda expression into one using de Bruijn
-- indexing.
convertWith :: Parser.Lambda -> Reader ConvertEnv Lambda
convertWith (x :~> lam) =
    abs x <$> local ((+1) *** (x:)) (convertWith lam)
convertWith (l :$: r) =
    app <$> convertWith l <*> convertWith r
convertWith (Pure (Var x)) =
    maybe (freeVar x) absVar . L.elemIndex x <$> asks snd
convertWith (Pure a) =
    return (Pure (Right a))

data ReversionError = VarNotFound Int
    deriving (Show, Eq)

-- | Converts a de Bruijn indexed lambda into a textual lambda. This function
-- assumes an empty context and a current binding depth of 0. If the expression
-- is not well formed (eg, there are abstracted variables that don't correspond
-- to anything in the context) then the function returns @Nothing@.
--
-- >>> revert (app (Types.abs "x" (absVar 0)) (freeVar "y"))
-- Right (Free (App (Free (Abs "x" (Pure (Var "x")))) (Pure (Var "y"))))
-- >>> revert (Types.abs "x" (Types.abs "y" (absVar 0)))
-- Right (Free (Abs "x" (Free (Abs "y" (Pure (Var "y"))))))
revert :: Lambda -> Either ReversionError Parser.Lambda
revert = flip runReader (0, mempty) . runExceptT . revertWith

type RevertEnv = (Int, [Text])

-- | Converts a de Bruijn indexed lambda into a textual lambda. Provide the
-- binding depth of the top most lambda and a context of bound variables to the
-- reader function.
revertWith :: Lambda -> ExceptT ReversionError (Reader RevertEnv) Parser.Lambda
revertWith (Free (Abs x e)) =
    abs x <$> local (\(d, b) -> (d + 1, x:b)) (revertWith e)
revertWith (Free (App l r)) =
    app <$> revertWith l <*> revertWith r
revertWith (Pure (Left hops)) = do
    b <- asks snd
    let r = maybe (Left (VarNotFound hops)) Right (listToMaybe $ drop hops b)
    var <$> ExceptT (return r)
revertWith (Pure (Right a)) = return (Pure a)

-- | Determines if two lambda expressions are alpha equivalent. Returns @Just
-- lambda@ if the two are equivalent, and @Nothing@ if they're not.
--
-- This is equivalent to @(==)@.
--
-- >>> alphaEquiv (Types.abs "x" (absVar 1)) (Types.abs "y" (absVar 1))
-- Just (Free (Abs "x" (Pure (Left 1))))
-- >>> alphaEquiv (Types.abs "x" (absVar 1)) (Types.abs "y" (freeVar "x"))
-- Nothing
alphaEquiv :: Lambda -> Lambda -> Maybe Lambda
alphaEquiv l@(Pure (Right (Var x))) (Pure (Right (Var y)))
    | x == y    = Just l
    | otherwise = Nothing
alphaEquiv l@(Pure (Left d)) (Pure (Left d'))
    | d == d'   = Just l
    | otherwise = Nothing
alphaEquiv (Free (App l r)) (Free (App l' r')) = do
    app <$> alphaEquiv l l' <*> alphaEquiv r r'
alphaEquiv (Free (Abs x e)) (Free (Abs _ f)) = do
    abs x <$> alphaEquiv e f
alphaEquiv _ _ = Nothing

-- | Retrives the set of free variables in a given lambda expression.
--
-- >>> freeVariables (Types.abs "x" (freeVar "y"))
-- fromList ["y"]
-- >>> freeVariables (app (absVar 0) (absVar 1))
-- fromList []
freeVariables :: Lambda -> Set Text
freeVariables = iter f . fmap g
  where
    f = \case
        App f x -> f <> x
        Abs _ m -> m
    g = \case
        Right (Var x) -> Set.singleton x
        _ -> mempty

-- | Given a pairing between a free variable name and a lambda, substitute the
-- lambda expression for each occurrence of the variable name.
--
-- >>> substitute ("x", freeVar "y") (freeVar "x")
-- Pure (Right (Var "y"))
-- >>> substitute ("x", freeVar "y") (freeVar "m")
-- Pure (Right (Var "m"))
substitute :: (Text, Lambda) -> Lambda -> Lambda
substitute (v, e) expr =
    expr >>= \case
        Right (Var n) | n == v -> e
        a -> return a

-- | Recursively reduce a lambda expression.
--
-- >>> betaReduction (app (Types.abs "x" (absVar 0)) (freeVar "x"))
-- Pure (Right (Var "x"))
betaReduction :: Lambda -> Lambda
betaReduction = flip runReader (0, mempty) . betaReductionWith

type ReduceEnv = (Int, Map Int Lambda)

betaReductionWith :: Lambda -> Reader ReduceEnv Lambda
betaReductionWith f@(Pure (Right _)) = return f
betaReductionWith (Pure (Left n)) = do
    (currDepth, bindings) <- ask
    return (fromMaybe (absVar n) (Map.lookup (n - currDepth) bindings))
betaReductionWith (Free (Abs n expr)) = do
    abs n <$> local (\(d, b) -> (d + 1, b)) (betaReductionWith expr)
betaReductionWith (Free (App (Free (Abs _ x)) r)) =
    local (\(d, b) -> (d, Map.insert d r b)) (betaReductionWith x)
betaReductionWith (Free (App l r)) =
    app <$> betaReductionWith l <*> betaReductionWith r

fullyReduce :: Lambda -> Lambda
fullyReduce = go 1000
  where
    go :: Int -> Lambda -> Lambda
    go 0 l = l
    go n l = let b = betaReduction l
              in if b == l then l else go (n - 1) b

type EvalEnv = Map Text Lambda

data EvalError
    = VariableNotFound Text
    | TypeMismatch Text Lambda
    deriving (Eq, Show)

eval :: Lambda -> EvalEnv -> Either EvalError Lambda
eval l e = runReaderT (go l) e
  where
    go :: Lambda -> ReaderT EvalEnv (Either EvalError) Lambda
    go (Free (App f x)) = app <$> go f <*> go x
    go (Free (Abs _ n)) = go n
    go (Pure (Right (Var x))) = do
        res <- asks (Map.lookup x)
        maybe (lift . Left . VariableNotFound $ x) return res
    go r@(Pure _) = return r


