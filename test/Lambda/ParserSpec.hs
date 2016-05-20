{-# LANGUAGE OverloadedStrings #-}

module Lambda.ParserSpec where

import Test.QuickCheck
import Test.Hspec
import Text.Megaparsec

import Data.Maybe
import Lambda.Parser

spec :: Spec
spec = do
    describe "var" $ do
        let p = parseMaybe variable
        it "accepts alphanumeric characters" $ do
            p "hello" `shouldBe` Just "hello"
        it "fails on symbols" $ do
            p "-asdf" `shouldBe` Nothing
    describe "abstraction" $ do
        let p = parseMaybe abstraction
        it "can parse an abstraction" $ do
            p "\\x. x" `shouldSatisfy` isJust
    describe "lambda" $ do
        let p = parseMaybe lambda
        it "can parse an application" $ do
            p "x y" `shouldBe` Just (App (Var "x") (Var "y"))
