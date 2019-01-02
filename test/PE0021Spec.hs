module PE0021Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0021

import Control.Monad (forM_)

amicableNumbersTests :: [(Int, Int, Bool)]
amicableNumbersTests =
    [ (220, 284, True)
    , (284, 220, True)
    , (19, 23, False)
    , (10, 9, False)
    , (10, 20, False)
    ]

amicableNumberTests :: [(Int, Bool)]
amicableNumberTests =
    [ (220, True)
    , (284, True)
    , (19, False)
    , (10, False)
    , (6, False)
    ]

spec :: Spec
spec = do
    describe "amicableNumbers" $
        forM_ amicableNumbersTests $ \(a, b, result) ->
            it ("gives " ++ show result ++ " for " ++ show a ++ "/" ++ show b) $
                amicableNumbers a b `shouldBe` result

    describe "amicableNumber" $
        forM_ amicableNumberTests $ \(a, result) ->
            it ("gives " ++ show result ++ " for " ++ show a) $
                amicableNumber a `shouldBe` result
