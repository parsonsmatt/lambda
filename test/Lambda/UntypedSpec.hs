module Lambda.UntypedSpec where

import Test.QuickCheck
import Test.Hspec

import Lambda.Untyped

spec :: Spec
spec =
    it "trivial" $ True `shouldBe` True
