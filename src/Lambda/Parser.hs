{-# LANGUAGE OverloadedStrings #-}

module Lambda.Parser where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
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

data Lambda
    = Var Text
    | App Lambda Lambda
    | Abs Text Lambda
    deriving Show

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (void spaceChar)
        (L.skipLineComment "--")
        (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse a lambda expression.
--
-- >>> let p = (\(Just x) -> x) . parseMaybe lambda
-- >>> p "x"
-- Var "x"
-- >>> p "(x y)"
-- App (Var "x") (Var "y")
-- >>> p "\\ x . (x x)"
-- Abs "x" (App (Var "x") (Var "x"))
lambda :: Parser Lambda
lambda = choice
    [ abstraction
    , parenApplication
    , manyApplication
    , Var <$> variable
    ]
  where
    parenApplication =
        lexeme $ between oparen cparen $ do
            l <- lexeme lambda
            r <- lexeme lambda
            return (App l r)
    manyApplication =
        some $ do


variable :: Parser Text
variable = T.pack <$> lexeme (some alphaNumChar)

abstraction :: Parser Lambda
abstraction = do
    slash
    v <- variable
    dot
    l <- lambda
    return (Abs v l)

consume :: Parser a -> Parser ()
consume = void . lexeme

slash :: Parser ()
slash = consume (char '\\')

dot :: Parser ()
dot = consume (char '.')

oparen :: Parser ()
oparen = consume (char '(')

cparen :: Parser ()
cparen = consume (char ')')
