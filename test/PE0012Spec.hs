module PE0012Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0012

spec :: Spec
spec = do
    describe "triangleNumber" $
        it "first 10 triangle numbers are [1,3,6,10,15,21,28,36,45,55]" $
            [triangleNumber x | x <- [1..10]]
                `shouldBe` [1,3,6,10,15,21,28,36,45,55]

    describe "factors" $
        it "gives [2,3,5,7,11,13] for 30030" $
            factors (product [2,3,5,7,11,13]) `shouldBe` [2,3,5,7,11,13]

    describe "numberOfDivisors" $
        it "gives [1,2,4,4,4,4,6] for [1,3,6,10,15,21,28]" $
            numberOfDivisors <$> [1,3,6,10,15,21,28] `shouldBe` [1,2,4,4,4,4,6]

    describe "findMinDivisors" $
        it "gives Just 28 for 5" $
            findMinDivisors 5 `shouldBe` Just 28