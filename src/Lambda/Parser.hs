{-# LANGUAGE OverloadedStrings #-}

module Lambda.Parser where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Text

import Data.Text (Text)
import qualified Data.Text as T

{-
Lambda calculus abstract grammar:

Lambda
    = Var
    | Abstraction
    | Application

Abstraction
    = \ Var . Lambda

Application
    = ( Lambda Lambda )

Var = alphanumeric
-}

type E = Text

data Lambda
    = Var Text
    | App Lambda Lambda
    | Abs Text Lambda
    deriving Show

-- | Parse a lambda expression.
--
-- >>> let p = (\(Just x) -> x) . parseMaybe lambda
-- >>> p "x"
-- Var "x"
-- >>> p "(x y)"
-- App (Var "x") (Var "y")
-- >>> p "\\ x . (x x)"
-- Abs "x" (App (Var "x") (Var "x"))
-- >>> p "\\x.\\y.(x(y(x y)))"
-- Abs "x" (Abs "y" (App (Var "x") (App (Var "y") (App (Var "x") (Var "y")))))
lambda :: Parser Lambda
lambda = do
    space
    (Var <$> variable) <|> abstraction <|> application
  where
    variable = do
        tok $ T.pack <$> some alphaNumChar
    abstraction = do
        slash
        v <- variable
        dot
        l <- lambda
        return (Abs v l)
    application =
        tok $ between oparen cparen $ do
            l <- tok lambda
            r <- tok lambda
            return (App l r)

tok :: Parser a -> Parser a
tok p = p <* space

slash :: Parser ()
slash = void (tok (char '\\'))

dot :: Parser ()
dot = void (tok (char '.'))

oparen :: Parser ()
oparen = void (tok (char '('))

cparen :: Parser ()
cparen = void (tok (char ')'))
