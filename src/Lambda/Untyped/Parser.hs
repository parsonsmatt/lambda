{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped.Parser where

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
    | Lit Literal
    deriving (Eq, Show)

data Literal
    = Str Text
    | Int Integer
    deriving (Eq, Show)

instance Arbitrary Lambda where
    arbitrary = sized go
      where
        go :: Int -> Gen Lambda
        go i
            | i <= 0    =
                oneof [ Var <$> randChars
                      , Lit . Str <$> randChars
                      , Lit . Int <$> arbitrary
                      ]
            | otherwise =
                oneof [ Abs <$> randChars <*> go (i - 1)
                      , App <$> go (i - 1) <*> go (i - 1)
                      , Var <$> randChars
                      ]
        randChars = T.pack . take 6 <$> listOf1 (choose ('a', 'z'))

    shrink (App a b) = [a, b] <> (uncurry App <$> shrink (a, b))
    shrink (Abs _ l) = l : shrink l
    shrink (Var a) = [Var a]
    shrink (Lit _) = []

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
                Lit {} -> pretty r
                _ -> "(" <> pretty r <> ")"
        Lit {} -> pretty l <> " " <>
            case r of
                 _ -> pretty r
pretty (Lit (Str a)) = "\"" <> a <> "\""
pretty (Lit (Int i)) = T.pack . show $ i

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
    , Lit <$> literal
    ]
  where
    manyApplication =
        foldl1 App <$> some (choice [
            abstraction
            , variable'
            , parens lambda
            , Lit <$> literal
            ])

-- | A parser for the fully explicit lambda calculus.
lambdaExplicit :: Parser Lambda
lambdaExplicit = choice
    [ variable'
    , Lit <$> literal
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

-- | Pretty-print a lambda expression with explicit parentheses.
prettyExplicit :: Lambda -> Text
prettyExplicit (Var a) = a
prettyExplicit (Abs v e) =
    "(\\" <> v <> " . " <> prettyExplicit e <> ")"
prettyExplicit (App l r) =
    "(" <> prettyExplicit l <> " " <> prettyExplicit r <> ")"
prettyExplicit (Lit r) =
    case r of
         Str x -> "\"" <> x <> "\""
         Int i -> T.pack . show $ i

parens :: Parser a -> Parser a
parens = between oparen cparen

literal :: Parser Literal
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
variable' = fmap Var variable

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
