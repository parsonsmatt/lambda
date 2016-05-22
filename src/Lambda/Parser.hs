{-# LANGUAGE OverloadedStrings #-}

module Lambda.Parser where

import Data.Monoid
import Control.Monad
import Text.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Gen ()
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
    deriving (Eq, Show)

instance Arbitrary Lambda where
    arbitrary = sized go
      where
        go :: Int -> Gen Lambda
        go i
            | i <= 0    = Var <$> randChars
            | otherwise =
                oneof [ Abs <$> randChars <*> go (i - 1)
                      , App <$> go (i - 1) <*> go (i - 1)
                      , Var <$> randChars
                      ]
        randChars = T.pack <$> listOf1 (choose ('a', 'z'))
    shrink (Var a) = [Var a]
    shrink (App a b) = [a, b] <> shrink a <> shrink b
    shrink (Abs _ l) = l : shrink l

-- | Pretty-prints a lambda expression.
-- >>> pretty (Var "x")
-- "x"
-- >>> pretty (App (App (Var "x") (Var "y")) (Var "z"))
-- "x y z"
-- >>> pretty (App (Var "x") (App (Var "y") (Var "z")))
-- "x (y z)"
-- >>> pretty (Abs "x" (Var "x"))
-- "\\x . x"
pretty :: Lambda -> Text
pretty (Var a) = a
pretty (Abs a l) = "\\" <> a <> " . " <> pretty l
pretty (App l@Var{} r@Var{}) = pretty l <> " " <> pretty r
pretty (App l r) =
    case l of
        Var {} ->
            case r of
                Var {} -> pretty l <> " " <> pretty r
                _ -> pretty l <> " (" <> pretty r <> ")"
        Abs {} -> "(" <> pretty l <> ") " <>
            case r of
                Var {} -> pretty r
                _ -> "(" <> pretty r <> ")"
        App {} -> pretty l <> " " <>
            case r of
                Var {} -> pretty r
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
    , parenApp
    , abstraction
    ]
  where
    manyApplication =
        foldl1 App <$> some (abstraction <|> fmap Var variable <|> parenApp)
    parenApp =
        between oparen cparen lambda


lambdaExplicit :: Parser Lambda
lambdaExplicit = choice
    [ fmap Var variable
    , parens $ choice
        [ do
          slash
          v <- variable
          dot
          e <- lambdaExplicit
          return (Abs v e)
        , App <$> lambdaExplicit <*> lambdaExplicit
        ]
    ]

parens :: Parser a -> Parser a
parens = between oparen cparen

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
