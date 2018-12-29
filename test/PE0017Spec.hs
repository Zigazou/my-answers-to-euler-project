module PE0017Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0017

import Control.Monad (forM_)

-- https://simple.wikipedia.org/wiki/Names_of_numbers_in_English
-- https://simple.wikipedia.org/wiki/Names_for_large_numbers
examples :: Integral a => Flavour -> [(a, String)]
examples British =
    [ (21, "twenty-one")
    , (29, "twenty-nine")
    , (64, "sixty-four")
    , (99, "ninety-nine")
    , (40, "forty")
    , (41, "forty-one")
    , (101, "one hundred and one")
    , (115, "one hundred and fifteen")
    , (102, "one hundred and two")
    , (175, "one hundred and seventy-five")
    , (200, "two hundred")
    , (300, "three hundred")
    , (342, "three hundred and forty-two")
    , (512, "five hundred and twelve")
    , (987, "nine hundred and eighty-seven")
    , (1984, "one thousand nine hundred and eighty-four")
    , (600000000000000000000000000, "six hundred quadrillion")
    , (600000000000000000000000001, "six hundred quadrillion and one")
    , (600000000000000000000000101, "six hundred quadrillion one hundred \
                                    \and one")
    , (765476250000000, "seven hundred and sixty-five billion four hundred \
                        \and seventy-six milliard two hundred and fifty \
                        \million")
    , (145000, "one hundred and forty-five thousand")
    , (10 ^ 105, "one septendecilliard")
    , (10 ^ 105 + 1984, "one septendecilliard one thousand nine hundred and \
                        \eighty-four")
    ]
examples American =
    [ (21, "twenty-one")
    , (29, "twenty-nine")
    , (64, "sixty-four")
    , (99, "ninety-nine")
    , (40, "forty")
    , (41, "forty-one")
    , (101, "one hundred one")
    , (102, "one hundred two")
    , (115, "one hundred fifteen")
    , (175, "one hundred seventy-five")
    , (200, "two hundred")
    , (300, "three hundred")
    , (342, "three hundred forty-two")
    , (512, "five hundred twelve")
    , (987, "nine hundred eighty-seven")
    , (1984, "one thousand nine hundred eighty-four")
    , (600000000000000000000000000, "six hundred septillion")
    , (600000000000000000000000001, "six hundred septillion one")
    , (600000000000000000000000101, "six hundred septillion one hundred one")
    , (765476250000000, "seven hundred sixty-five trillion four hundred \
                        \seventy-six billion two hundred fifty million")
    , (145000, "one hundred forty-five thousand")
    , (10 ^ 105, "one quattuortrigintillion")
    , (10 ^ 105 + 1984, "one quattuortrigintillion one thousand nine hundred \
                        \eighty-four")
    ]

groupDigitsTests :: Integral a => [(a, [(a,a)])]
groupDigitsTests =
    [ (0, [])
    , (1, [(0, 1)])
    , (10, [(0, 10)])
    , (123, [(2, 1), (0, 23)])
    , (1000, [(3, 1)])
    , (999999, [(3, 999), (2, 9), (0, 99)])
    , (1000000, [(6, 1)])
    , (999999999, [(6, 999), (3, 999), (2, 9), (0, 99)])
    ]

spec :: Spec
spec = do
    describe "groupDigits" $
        forM_ groupDigitsTests $ \(value, result) ->
            it ("gives " ++ show result ++ " for " ++ show value) $
                groupDigits value `shouldBe` result

    describe "numberToEnglish (British)" $
        forM_ (examples British) $ \(value, result) ->
            it ("gives " ++ show result ++ " for " ++ show value) $
                numberToEnglish British value `shouldBe` result

    describe "numberToEnglish (American)" $
        forM_ (examples American) $ \(value, result) ->
            it ("gives " ++ show result ++ " for " ++ show value) $
                numberToEnglish American value `shouldBe` result

    describe "countLetters " $ do
        it "gives 23 for 342" $ 
            countLetters (numberToEnglish British 342) `shouldBe` 23

        it "gives 20 for 115" $ 
            countLetters (numberToEnglish British 115) `shouldBe` 20
