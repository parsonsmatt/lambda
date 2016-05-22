{-# LANGUAGE OverloadedStrings #-}

module Decl.ParserSpec where

import System.Directory
import Data.Monoid
import Test.QuickCheck
import Test.Hspec
import Text.Megaparsec hiding (fromFile)
import Data.Either
import Text.Megaparsec.Text

import Lambda.Parser (Lambda(..), lambda)
import Decl.Parser

declaration' :: Parser (Decl Lambda)
declaration' = declaration lambda

spec :: Spec
spec = do
    describe "declaration" $ do
        let p = parse declaration' ""
        it "parses a declaration" $ do
            p "def foo = x;"
                `shouldBe`
                    Right (Def "foo" (Var "x"))
    describe "file examples" $ do
        let path = "test/Examples/parse/decl/"
            path' i = path <> i <> ".lm"
        let p f = fromFile f lambda
        it "parses decl-00.lm" $ do
            p (path' "decl-00") >>= (`shouldSatisfy` isRight)
