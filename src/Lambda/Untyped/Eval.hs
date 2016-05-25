{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped.Eval where

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans ()
-- import qualified Data.Text as T
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Lambda.Untyped.Parser as Parser

data Lambda
    = FreeVar Text
    | AbsVar Int
    | App Lambda Lambda
    | Abs Text Lambda
    deriving (Eq, Show)

-- | Converts a parsed Lambda expression to an expression in de Bruijn notation.
--
-- >>> :set -XOverloadedStrings
-- >>> convert (Parser.Var "x")
-- FreeVar "x"
-- >>> convert (Parser.Abs "x" (Parser.Var "x"))
-- Abs "x" (AbsVar 0)
-- >>> convert (Parser.Abs "x" (Parser.App (Parser.Var "x") (Parser.Var "x")))
-- Abs "x" (App (AbsVar 0) (AbsVar 0))
convert :: Parser.Lambda -> Lambda
convert = flip runReader (0, []) . convertWith

type ConvertEnv = (Int, [Text])

-- | Given an environment consisting of the current binding depth and a list of
-- bound variables, converts a parsed lambda expression into one using de Bruijn
-- indexing.
convertWith :: Parser.Lambda -> Reader ConvertEnv Lambda
convertWith (Parser.Abs x lam) = do
    Abs x <$> local (\(d, xs) -> (d + 1, x:xs)) (convertWith lam)
convertWith (Parser.App l r) = do
    App <$> convertWith l <*> convertWith r
convertWith (Parser.Var x) = do
    env <- asks snd
    return $ maybe (FreeVar x) AbsVar (L.elemIndex x env)

data ReversionError = VarNotFound Int
    deriving (Show, Eq)

-- | Converts a de Bruijn indexed lambda into a textual lambda. This function
-- assumes an empty context and a current binding depth of 0. If the expression
-- is not well formed (eg, there are abstracted variables that don't correspond
-- to anything in the context) then the function returns @Nothing@.
--
-- >>> revert (App (Abs "x" (AbsVar 0)) (FreeVar "y"))
-- Right (App (Abs "x" (Var "x")) (Var "y"))
-- >>> revert (Abs "x" (Abs "y" (AbsVar 0)))
-- Right (Abs "x" (Abs "y" (Var "y")))
revert :: Lambda -> Either ReversionError Parser.Lambda
revert = flip runReader (0, mempty) . runExceptT . revertWith

type RevertEnv = (Int, Map Int Text)

-- | Converts a de Bruijn indexed lambda into a textual lambda. Provide the
-- binding depth of the top most lambda and a context of bound variables to the
-- reader function.
revertWith :: Lambda -> ExceptT ReversionError (Reader RevertEnv) Parser.Lambda
revertWith (FreeVar x) = return (Parser.Var x)
revertWith (AbsVar hops) = do
    (depth, b) <- ask
    let r = maybe (Left (VarNotFound hops)) Right (Map.lookup (hops ) b)
    Parser.Var <$> ExceptT (return r)
revertWith (Abs x e) =
    Parser.Abs x <$> local (\(d, b) -> (d + 1, Map.insert d x b)) (revertWith e)
revertWith (App l r) =
    Parser.App <$> revertWith l <*> revertWith r


-- | Determines if two lambda expressions are alpha equivalent. Returns @Just
-- lambda@ if the two are equivalent, and @Nothing@ if they're not.
--
-- >>> alphaEquiv (Abs "x" (AbsVar 1)) (Abs "y" (AbsVar 1))
-- Just (Abs "x" (AbsVar 1))
-- >>> alphaEquiv (Abs "x" (AbsVar 1)) (Abs "y" (FreeVar "x"))
-- Nothing
alphaEquiv :: Lambda -> Lambda -> Maybe Lambda
alphaEquiv (FreeVar x) (FreeVar y)
    | x == y    = Just (FreeVar x)
    | otherwise = Nothing
alphaEquiv (AbsVar d) (AbsVar d')
    | d == d'   = Just (AbsVar d)
    | otherwise = Nothing
alphaEquiv (App l r) (App l' r') = do
    App <$> alphaEquiv l l' <*> alphaEquiv r r'
alphaEquiv (Abs x e) (Abs _ f) = do
    Abs x <$> alphaEquiv e f
alphaEquiv _ _ = Nothing

-- | Retrives the set of free variables in a given lambda expression.
--
-- >>> freeVariables (Abs "x" (FreeVar "y"))
-- fromList ["y"]
-- >>> freeVariables (App (AbsVar 0) (AbsVar 1))
-- fromList []
freeVariables :: Lambda -> Set Text
freeVariables (FreeVar x) = Set.singleton x
freeVariables (AbsVar _) = mempty
freeVariables (Abs _ e) = freeVariables e
freeVariables (App l r) = freeVariables l <> freeVariables r

-- | Given a pairing between a free variable name and a lambda, substitute the
-- lambda expression for each occurrence of the variable name.
substitute :: (Text, Lambda) -> Lambda -> Lambda
substitute s@(var, expr) v =
    case v of
         AbsVar _ -> v
         App l r -> App (substitute s l) (substitute s r)
         FreeVar n -> if n == var then expr else v
         Abs n e -> Abs n (substitute s e)

-- | Recursively reduce a lambda expression.
--
-- >>> betaReduction (App (Abs "x" (AbsVar 0)) (FreeVar "x"))
-- FreeVar "x"
betaReduction :: Lambda -> Lambda
betaReduction = flip runReader (0, mempty) . betaReductionWith

type ReduceEnv = (Int, Map Int Lambda)

betaReductionWith :: Lambda -> Reader ReduceEnv Lambda
betaReductionWith f@(FreeVar _) = return f
betaReductionWith (AbsVar n) = do
    (currDepth, bindings) <- ask
    return (fromMaybe (AbsVar n) (Map.lookup (n - currDepth) bindings))
betaReductionWith (Abs n expr) = do
    Abs n <$> local (\(d, b) -> (d + 1, b)) (betaReductionWith expr)
betaReductionWith (App (Abs _ x) r) =
    local (\(d, b) -> (d, Map.insert d r b)) (betaReductionWith x)
betaReductionWith (App l r) =
    App <$> betaReductionWith l <*> betaReductionWith r

fullyReduce :: Lambda -> Lambda
fullyReduce = go 1000
  where
    go :: Int -> Lambda -> Lambda
    go 0 l = l
    go n l = let b = betaReduction l
              in if b == l then l else go (n - 1) b

type EvalEnv = Map Text Lambda

eval :: Lambda -> EvalEnv -> Maybe Lambda
eval l e = runReaderT (go l) e
  where
    go :: Lambda -> ReaderT EvalEnv Maybe Lambda
    go (FreeVar x) = do
        env <- ask
        lift (Map.lookup x env)
    go r@(AbsVar _) = return r
    go (App f x) = App <$> go f <*> go x
    go (Abs _ n) = go n


