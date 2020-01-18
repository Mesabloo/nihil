module Nihil.C_DesugarerSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "hello desugarer" (it "should work" (0 `shouldBe` 0))