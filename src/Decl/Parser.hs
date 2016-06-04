{-# LANGUAGE OverloadedStrings #-}
module Decl.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

data Decl expr
    = Def Text expr
    deriving (Eq, Show)

declaration :: Parser expr -> Parser (Decl expr)
declaration pexpr = do
    defKw
    name <- T.pack <$> lexeme (some alphaNumChar)
    expr <- between (lexeme (char '=')) (lexeme (char ';')) pexpr
    return (Def name expr)

declarations :: Parser expr -> Parser [Decl expr]
declarations = many . lexeme . declaration

fromFile :: FilePath -> Parser expr -> IO (Either (ParseError Char Dec) [Decl expr])
fromFile p e = do
    s <- T.readFile p
    return (parse (declarations e) p s)


spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (void spaceChar)
        (L.skipLineComment "--")
        (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

consume :: Parser a -> Parser ()
consume = void . lexeme

defKw :: Parser ()
defKw = consume (string "def")
