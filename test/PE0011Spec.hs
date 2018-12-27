module PE0011Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0011

spec :: Spec
spec = do
    describe "diagonals" $
        it "gives [[1],[3,2],[4]] for [[1,2],[3,4]]" $
            diagonals [[1,2],[3,4]] `shouldBe` [[1],[3,2],[4]]

    describe "seqWithoutZero" $ do
        it "gives [[1,2],[3,4]] for [[1,2],[3,4]]" $
            seqWithoutZero 2 [[1,2],[3,4]] `shouldBe` [[1,2],[3,4]]

        it "gives [[3,4]] for 2 and [[1,0],[3,4]]" $
            seqWithoutZero 2 [[1,0],[3,4]] `shouldBe` [[3,4]]

    describe "fixedLength" $ do
        it "gives [[1,2],[2,3]] for 2 and [1,2,3]" $
            fixedLength 2 [1,2,3] `shouldBe` [[1,2],[2,3]]

        it "gives [] for 3 and [1,2]" $
            fixedLength 3 [1,2] `shouldBe` []

    describe "largestProduct" $ do
        it "gives 12 for 2 and [[1,2],[3,4]]" $
            largestProduct 2 [[1,2],[3,4]] `shouldBe` 12

        it "gives 12 for 2 and [[1,0],[3,4]]" $
            largestProduct 2 [[1,0],[3,4]] `shouldBe` 12

        it "gives 8 for 2 and [[1,2],[0,4]]" $
            largestProduct 2 [[1,2],[0,4]] `shouldBe` 8

        it "gives 4 for 2 and [[1,2],[2,1]]" $
            largestProduct 2 [[1,2],[2,1]] `shouldBe` 4
