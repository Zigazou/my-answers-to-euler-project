module PE0002Spec (spec) where

import Test.Hspec
import PE0002

spec :: Spec
spec = do
    describe "fibonacci" $
        it "first 10 elements are [ 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 ]" $
            take 10 fibonacci `shouldBe` [ 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 ]

    describe "sumFibonacci" $
        it "gives 44 for even values not exceeding 89" $
            sumFibonacci (<= 89) even `shouldBe` 44
