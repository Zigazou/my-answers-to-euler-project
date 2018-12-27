module PE0006Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0006

spec :: Spec
spec = describe "sumSquareDifference" $
        it "gives 2640 for [1..10]" $
            sumSquareDifference [1..10] `shouldBe` 2640