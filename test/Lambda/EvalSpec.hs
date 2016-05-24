{-# LANGUAGE OverloadedStrings #-}

module Lambda.EvalSpec where

import Test.Hspec
import Test.QuickCheck
-- import Data.Either

import qualified Lambda.Parser as P
import Lambda.Eval as E

spec :: Spec
spec = do
    describe "convert" $ do
        it "can convert any lambda expression" $ do
            property $ \x ->
                convert x === convert x
        it "converts id correctly" $ do
            let e = P.Abs "x" (P.Var "x")
            convert e `shouldBe` Abs "x" (AbsVar 0)
        it "converts const correctly" $ do
            let e = P.Abs "x" (P.Abs "y" (P.Var "x"))
            convert e
                `shouldBe`
                    Abs "x" (Abs "y" (AbsVar 1))
        it "converts flip const correctly" $ do
            let e = P.Abs "x" (P.Abs "y" (P.Var "y"))
            convert e
                `shouldBe`
                    Abs "x" (Abs "y" (AbsVar 0))
    describe "revert" $ do
        it "converts and reverts as isomorphism" $ do
            property $ \x ->
                revert (convert x) === pure x
    describe "beta reduction" $ do
        it "doesn't alter abstractions" $ do
            betaReduction (Abs "x" (FreeVar "y"))
                `shouldBe`
                    Abs "x" (FreeVar "y")
        it "doesn't alter free variables" $ do
            betaReduction (FreeVar "x")
                `shouldBe`
                    FreeVar "x"
        it "doesn't alter bound variables with empty context" $ do
            betaReduction (AbsVar 1)
                `shouldBe`
                    AbsVar 1
        it "reduces applications to abstractions" $ do
            betaReduction (App (Abs "x" (AbsVar 0)) (FreeVar "y"))
                `shouldBe`
                    FreeVar "y"
        it "handles nested applications" $ do
            betaReduction (App (Abs "x" (Abs "y" (AbsVar 1))) (FreeVar "z"))
                `shouldBe`
                    Abs "y" (FreeVar "z")
        it "inserts free vars" $ do
            betaReduction (App (Abs "x" (AbsVar 0)) (FreeVar "x"))
                `shouldBe`
                    FreeVar "x"
