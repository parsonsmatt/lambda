{-# LANGUAGE OverloadedStrings #-}

module Lambda.UntypedSpec where

import Control.Monad
import Test.QuickCheck
import Test.Hspec

import Lambda.Untyped
import Lambda.Untyped.Eval

spec :: Spec
spec = do
    describe "with standard library" $ do
        Right stdlib <- runIO . loadFromFile $ "std.lm"
        let eval' = fmap fullyReduce . flip evaluate stdlib
            e = eval' >=> eval' >=> eval' >=> eval'
        describe "arithmetic" $ do
            let zero = FreeVar "zero"
                plus = FreeVar "plus"
                one = FreeVar "one"
                succ = FreeVar "succ"
                otherOne = FreeVar "otherOne"
            it "succ zero == one" $ do
                e (App succ zero) `shouldBe` e one
            it "plus one zero == succ zero" $ do
                e (App (App plus one) zero) `shouldBe` e (App succ zero)
            it "plus one one == succ one" $ do
                pendingWith "the evaluation isn't strongly normalizing"
                e (App (App plus one) one) `shouldBe` e (App succ one)
            it "one == otherOne" $ do
                e one `shouldBe` e otherOne
