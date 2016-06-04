{-# LANGUAGE OverloadedStrings #-}

module Lambda.Untyped.FreeEvalSpec where

import Prelude hiding (abs)
import Test.Hspec
import Test.QuickCheck

import Lambda.Untyped.Types
import qualified Lambda.Untyped.Parser.Free as P
import Lambda.Untyped.Eval.Free as E

spec :: Spec
spec = do
    describe "convert" $ do
        it "can convert any lambda expression" $ do
            property $ \x ->
                convert x === convert x
        it "converts id correctly" $ do
            let e = abs "x" (var "x")
            convert e `shouldBe` abs "x" (absVar 0)
        it "converts const correctly" $ do
            let e = abs "x" (abs "y" (var "x"))
            convert e
                `shouldBe`
                    abs "x" (abs "y" (absVar 1))
        it "converts flip const correctly" $ do
            let e = abs "x" (abs "y" (var "y"))
            convert e
                `shouldBe`
                    abs "x" (abs "y" (absVar 0))
        it "converts an app correctly" $ do
            let e = abs "x" (app (var "x") (var "x"))
            convert e
                `shouldBe`
                    abs "x" (app (absVar 0) (absVar 0))

    describe "revert" $ do
        it "converts and reverts as isomorphism" $ do
            property $ \x ->
                fmap P.pretty (revert (convert x)) === pure (P.pretty x)
        describe "nested abstractions" $ do
            let input = abs "r" (abs "m" (var "m"))
                converted = abs "r" (abs "m" (absVar 0))
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
            betaReduction (abs "x" (freeVar "y"))
                `shouldBe`
                    abs "x" (freeVar "y")
        it "doesn't alter free variables" $ do
            betaReduction (freeVar "x")
                `shouldBe`
                    freeVar "x"
        it "doesn't alter bound variables with empty context" $ do
            betaReduction (absVar 1)
                `shouldBe`
                    absVar 1
        it "reduces applications to abstractions" $ do
            betaReduction (app (abs "x" (absVar 0)) (freeVar "y"))
                `shouldBe`
                    freeVar "y"
        it "handles nested applications" $ do
            betaReduction (app (abs "x" (abs "y" (absVar 1))) (freeVar "z"))
                `shouldBe`
                    abs "y" (freeVar "z")
        it "inserts free vars" $ do
            betaReduction (app (abs "x" (absVar 0)) (freeVar "x"))
                `shouldBe`
                    freeVar "x"
