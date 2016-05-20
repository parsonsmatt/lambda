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
            let iso = pretty . fromJust . parseMaybe lambda . pretty
            property $ \x -> do
               iso x === pretty x
        -- fails on
            let fail =
                   App (Abs "ewvvuh" (App (App (App (Var "rwavyjf") (Var "rn")) (App (Abs "lxhkcbi" (Var "sfaw")) (App (Abs "rnd" (Var "dyvsk")) (Var "tvqba")))) (Var "ypnqvd"))) (App (Var "rwnldi") (Abs "ixbmcs" (Abs "yi" (Abs "ghjxy" (Var "dmmfrhp")))))
                actual =
                    "(\\ewvvuh . rwavyjf rn (\\lxhkcbi . sfaw) (\\rnd . dyvsk) tvqba ypnqvd) rwnl di (\\ixbmcs . (\\yi . (\\ghjxy . dmmfrhp)))"
                attemp =
                    "(\\ewvvuh . rwavyjf rn (\\lxhkcbi . sfaw) (\\rnd . dyvsk) tvqba ypnqvd) rwnl di ((\\ixbmcs . (\\yi . (\\ghjxy . dmmfrhp))))"

    describe "pretty" $ do
        it "prints it nice" $ do
            let t = App (Abs "s" (Var "w")) (Var "j")
            pretty t `shouldBe` "(\\s . w) j"
