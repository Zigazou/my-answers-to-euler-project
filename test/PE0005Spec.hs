module PE0005Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0005

spec :: Spec
spec = do
    describe "smallestMultiple" $
        it "gives 2520 for [1..10]" $
            smallestMultiple [1..10] `shouldBe` 2520