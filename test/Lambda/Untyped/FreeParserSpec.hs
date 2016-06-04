{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped.FreeParserSpec where

import Prelude hiding (abs)

import Data.Either
import Test.QuickCheck
import Test.Hspec
import Text.Megaparsec

import Data.Maybe
import Lambda.Untyped.Types
import Lambda.Untyped.Parser.Free

spec :: Spec
spec = do
    describe "lit" $ do
        let p = parse literal ""
        it "can parse a string" $ do
            p "\"foobar\"" `shouldBe` pure (Str "foobar")
        it "can parse a number" $ do
            p "1234" `shouldBe` pure (Int 1234)
        it "can parse a negative number" $ do
            p "-123" `shouldBe` pure (Int (-123))

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
        let p = parse lambda "test"
        it "can parse a literal string" $ do
            p "\"foobar\"" `shouldBe` pure (str "foobar")
        it "can parse an integer literal" $ do
            p "1234" `shouldBe` pure (int 1234)
        it "can parse an application" $ do
            p "x y" `shouldBe` pure (app (var "x") (var "y"))
        it "can parse a bunch of application" $ do
            p "x y (x y)" `shouldSatisfy` isRight
        it "can parse a whole mess of stuff" $ do
            p "\\x . \\y . x (x y) (y x)" `shouldSatisfy` isRight
        it "can parse expressions beginning with parens" $ do
            p "(\\x . y)" `shouldSatisfy` isRight
            p "(\\x . y) a" `shouldSatisfy` isRight
        it "handles parens" $ do
            p "(x y) x" `shouldBe` pure (app (app (var "x") (var "y")) (var "x"))
        describe "with literals" $ do
            it "handles application of literal" $ do
                p "f 100" `shouldBe` pure (app (var "f") (int 100))
            it "handles abstraction on literal" $ do
                p "(\\x . x) 100" `shouldBe`
                    pure (app (abs "x" (var "x")) (int 100))
            it "does weird things" $ do
                p "\"hello\" 100" `shouldBe`
                    pure (app (str "hello") (int 100))
        it "preserves Galois connection" $ do
            let iso = parse lambda "test" . pretty
            property $ \x -> do
                iso x === pure x

    describe "pretty" $ do
        it "prints it nice" $ do
            let t = app (abs "s" (var "w")) (var "j")
            pretty t `shouldBe` "(\\s . w) j"
        it "handles application streams" $ do
            let t = app (app (app (var "x") (var "y")) (var "z")) (var "w")
            pretty t `shouldBe` "x y z w"
        it "parenthesizes appropriately" $ do
            let t = app (var "x") (app (var "y") (var "z"))
            pretty t `shouldBe` "x (y z)"

    describe "explicit" $ do
        let p = parse lambdaExplicit "test"
        it "parses variables" $ do
            p "a" `shouldBe` Right (var "a")
        it "parses applications" $ do
            p "(a b)" `shouldBe` Right (app (var "a") (var "b"))
        it "parses abstractions" $ do
            p "(\\a . a)" `shouldBe` Right (abs "a" (var "a"))
