module PE0014Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0014

spec :: Spec
spec = do
    describe "collatzSequence" $
        it "gives [13,40,20,10,5,16,8,4,2,1] for 13" $
            collatzSequence 13 `shouldBe` [13,40,20,10,5,16,8,4,2,1]

    describe "collatzLength" $
        it "gives [(13,10),(1,1),(2,2)] for [13,1,2]" $
            collatzLength [13,1,2] `shouldBe` [(13,10),(1,1),(2,2)]
