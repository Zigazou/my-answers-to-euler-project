module PE0001Spec (spec) where

import Test.Hspec
import PE0001

spec :: Spec
spec = do
    describe "multiplesOf" $ do
        it "gives the first 5 multiples of 0" $
            take 5 (multiplesOf 0) `shouldBe` [ 0, 0, 0, 0, 0 ]

        it "gives the first 5 multiples of 3" $
            take 5 (multiplesOf 3) `shouldBe` [ 3, 6, 9, 12, 15 ]

        it "gives the first 3 multiples of 5" $
            take 3 (multiplesOf 5) `shouldBe` [ 5, 10, 15 ]

    describe "union" $ do
        it "of [] and [] gives []" $
            union [] [] `shouldBe` ([] :: [Integer])

        it "of [ 1, 2, 3 ] and [] gives [ 1, 2, 3 ]" $
            union [ 1, 2, 3 ] [] `shouldBe` [ 1, 2, 3 ]

        it "of [] and [ 10, 20, 30 ] gives [ 10, 20, 30 ]" $
            union [] [ 10, 20, 30 ] `shouldBe` [ 10, 20, 30 ]

        it "of [ 1, 2, 3 ] and [ 10, 20, 30 ] gives [ 1, 2, 3, 10, 20, 30 ]" $
            union [ 1, 2, 3 ] [ 10, 20, 30 ] `shouldBe` [ 1, 2, 3, 10, 20, 30 ]

        it "of [ 1, 3, 5 ] and [ 2, 4, 6 ] gives [ 1, 2, 3, 4, 5, 6 ]" $
            union [ 1, 3, 5 ] [ 2, 4, 6 ] `shouldBe` [ 1, 2, 3, 4, 5, 6 ]

        it "of [ 1, 2, 3 ] and [ 1, 2, 3 ] gives [ 1, 2, 3 ]" $
            union [ 1, 2, 3 ] [ 1, 2, 3 ] `shouldBe` [ 1, 2, 3 ]

        it "of [ 6, 9, 12, 15 ] and [ 5, 15 ] gives [ 5, 6, 9, 12, 15 ]" $
            union [ 6, 9, 12, 15 ] [ 5, 15 ] `shouldBe` [ 5, 6, 9, 12, 15 ]

    describe "sumMultiplesOf" $
        it "gives 23 for the sum of multiples of 3 or 5 below 10" $
            sumMultiplesOf 3 5 10 `shouldBe` 23
