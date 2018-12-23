module PE0004Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0004

spec :: Spec
spec = do
    describe "isPalindromic" $ do
        it "says yes for 1, 11, 111, 121, 1221, 9009" $
            all isPalindromic [ 1, 11, 111, 121, 1221, 9009 ] `shouldBe` True

        it "says no for 23, 573, 2005, 90950" $
            any isPalindromic [ 23, 573, 2005, 90950 ] `shouldBe` False

    describe "fixedDigits" $ do
        it "gives [99,98..10] for 2" $
            fixedDigits 2 `shouldBe` [ 99, 98 .. 10 ]

        it "gives [999,998..100] for 3" $
            fixedDigits 3 `shouldBe` [ 999, 998 .. 100 ]

        it "gives [9,8..1] for 1" $
            fixedDigits 1 `shouldBe` [ 9, 8 .. 1 ]

    describe "allCouples" $ do
        it "gives [[(1,1)]] for [1]" $
            allCouples [1] `shouldBe` [[(1,1)]]

        it "gives [[(1,1),(1,2)],[(2,2)]] for [1,2]" $
            allCouples [1,2] `shouldBe` [[(1,1),(1,2)],[(2,2)]]

        it "gives [[(1,1),(1,2),(1,3)],[(2,2),(2,3)],[(3,3)]] for [1,2,3]" $
            allCouples [1,2,3] `shouldBe` [[(1,1),(1,2),(1,3)],[(2,2),(2,3)],[(3,3)]]

    describe "firstPalindrom" $ do
        it "gives Nothing for []" $
            firstPalindrom [] `shouldBe` Nothing

        it "gives Nothing for [(2,5),(10,3)]" $
            firstPalindrom (makeCoupleProduct <$> [(2,5),(10,3)])
                `shouldBe` Nothing

        it "gives Just 33 for [(13,3),(11,3),(9,3)]" $
            cpProduct <$> firstPalindrom (makeCoupleProduct <$> [(13,3),(11,3),(9,3)])
                `shouldBe` Just 33

    describe "biggestPalindrom" $ do
        it "gives Just 9 for 1 digit" $
            cpProduct <$> biggestPalindrom 1 `shouldBe` Just 9

        it "gives Just 9009 for 2 digits" $
            cpProduct <$> biggestPalindrom 2 `shouldBe` Just 9009
