{-# LANGUAGE OverloadedStrings #-}
module Lambda.Untyped.Parser.Free where

import Data.Monoid
import Control.Monad
import Text.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Gen ()
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T

import Lambda.Untyped.Types

type Lambda = Parsed Text

-- | Pretty-prints a lambda expression.
--
-- >>> pretty (Var "x")
-- "x"
-- >>> pretty (App (App (Var "x") (Var "y")) (Var "z"))
-- "x y z"
-- >>> pretty (App (Var "x") (App (Var "y") (Var "z")))
-- "x (y z)"
-- >>> pretty (Abs "x" (Var "x"))
-- "\\x . x"
-- >>> pretty (Lit (Str "hello"))
-- "\"hello\""
-- >>> pretty (Lit (Int 1000))
-- "1000"
-- >>> pretty (App (Lit (Str "foo")) (Lit (Int 100)))
-- "\"foo\" 100"
pretty :: Lambda -> Text
pretty (Pure (Var a)) = a
pretty (Free (Abs a l)) = "\\" <> a <> " . " <> pretty l
pretty (Free (App l@(Pure (Var{})) r@(Pure (Var{})))) = pretty l <> " " <> pretty r
pretty (Free (App l r)) =
    case l of
        Pure (Var {}) ->
            case r of
                Pure (Var {}) -> pretty l <> " " <> pretty r
                _ -> pretty l <> " (" <> pretty r <> ")"
        Free (Abs {}) -> "(" <> pretty l <> ") " <>
            case r of
                Pure (Var {}) -> pretty r
                _ -> "(" <> pretty r <> ")"
        Free (App {}) -> pretty l <> " " <>
            case r of
                Pure (Var {}) -> pretty r
                _ -> "(" <> pretty r <> ")"

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
-- >>> p "x y z"
-- App (App (Var "x") (Var "y")) (Var "z")
lambda :: Parser Lambda
lambda = choice
    [ manyApplication
    , parens lambda
    , abstraction
    , Pure <$> literal
    ]
  where
    manyApplication =
        foldl1 (\a c -> Free (App a c)) <$> some (choice [
            abstraction
            , variable'
            , parens lambda
            , Pure <$> literal
            ])

-- | A parser for the fully explicit lambda calculus.
lambdaExplicit :: Parser Lambda
lambdaExplicit = choice
    [ variable'
    , Pure <$> literal
    , fmap Free . parens $ choice
        [ do
          slash
          v <- variable
          dot
          e <- lambdaExplicit
          return (Abs v e)
        , App <$> lambdaExplicit <*> lambdaExplicit
        ]
    ]

-- | Pretty-print a lambda expression with explicit parentheses.
prettyExplicit :: Lambda -> Text
prettyExplicit (Pure (Var a)) = a
prettyExplicit (Free (Abs v e)) =
    "(\\" <> v <> " . " <> prettyExplicit e <> ")"
prettyExplicit (Free (App l r)) =
    "(" <> prettyExplicit l <> " " <> prettyExplicit r <> ")"

parens :: Parser a -> Parser a
parens = between oparen cparen

literal :: Parser (Literal Text)
literal = lexeme $ choice
    [ Int . read <$> choice
        [ (:) <$> char '-' <*> some digitChar
        , some digitChar
        ]
    , Str . T.pack <$> do
        char '"'
        L.charLiteral `manyTill` char '"'
    ]

variable :: Parser Text
variable = T.pack <$> lexeme ((:) <$> oneOf ['a'..'z'] <*> many alphaNumChar)

variable' :: Parser Lambda
variable' = fmap (Pure . Var) variable

abstraction :: Parser Lambda
abstraction = do
    slash
    v <- variable
    dot
    l <- lambda
    return (Free (Abs v l))

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
