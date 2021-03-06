{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped.EvalSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Lambda.Untyped.Parser as P
import Lambda.Untyped.Eval as E

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
        it "converts an app correctly" $ do
            let e = P.Abs "x" (P.App (P.Var "x") (P.Var "x"))
            convert e
                `shouldBe`
                    Abs "x" (App (AbsVar 0) (AbsVar 0))

    describe "revert" $ do
        it "converts and reverts as isomorphism" $ do
            property $ \x ->
                fmap P.pretty (revert (convert x)) === pure (P.pretty x)
        describe "nested abstractions" $ do
            let input = P.Abs "r" (P.Abs "m" (P.Var "m"))
                converted = Abs "r" (Abs "m" (AbsVar 0))
            it "convert input = converted" $ do
                convert input `shouldBe` converted
            it "revert converted = pure input" $ do
                revert converted `shouldBe` pure input
            it "prints the right thing" $ do
                fmap P.pretty (revert (convert input))
                    `shouldBe`
                        pure ("\\r . \\m . m")

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
