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
        it "can parse a bunch of application" $ do
            p "x y (x y)" `shouldSatisfy` isJust
        it "can parse a whole mess of stuff" $ do
            p "\\x . \\y . x (x y) (y x)" `shouldSatisfy` isJust
        it "can parse expressions beginning with parens" $ do
            p "(\\x . y)" `shouldSatisfy` isJust
            p "(\\x . y) a" `shouldSatisfy` isJust
        it "handles parens" $ do
            p "(x y) x" `shouldBe` Just (App (App (Var "x") (Var "y")) (Var "x"))
        it "preserves Galois connection" $ do
            let iso = parse lambda "test" . pretty
            property $ \x -> do
               iso x === Right x

    describe "pretty" $ do
        it "prints it nice" $ do
            let t = App (Abs "s" (Var "w")) (Var "j")
            pretty t `shouldBe` "(\\s . w) j"
        it "handles application streams" $ do
            let t = App (App (App (Var "x") (Var "y")) (Var "z")) (Var "w")
            pretty t `shouldBe` "x y z w"
        it "parenthesizes appropriately" $ do
            let t = App (Var "x") (App (Var "y") (Var "z"))
            pretty t `shouldBe` "x (y z)"

    describe "explicit" $ do
        let p = parse lambdaExplicit "test"
        it "parses variables" $ do
            p "a" `shouldBe` Right (Var "a")
        it "parses applications" $ do
            p "(a b)" `shouldBe` Right (App (Var "a") (Var "b"))
        it "parses abstractions" $ do
            p "(\\a . a)" `shouldBe` Right (Abs "a" (Var "a"))


