module PE0009Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0009

spec :: Spec
spec = do
    describe "isPythagorean" $ do
        it "gives False for Triplet 1 2 3" $
            isPythagorean (Triplet 1 2 3) `shouldBe` False

        it "gives False for Triplet 3 2 1" $
            isPythagorean (Triplet 3 2 1) `shouldBe` False

        it "gives True for Triplet 3 4 5" $
            isPythagorean (Triplet 3 4 5) `shouldBe` True

    describe "solution" $ do
        it "should not give Nothing" $ solution `shouldNotBe` Nothing
