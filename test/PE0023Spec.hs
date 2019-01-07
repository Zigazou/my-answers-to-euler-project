module PE0023Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0023

import Control.Monad (forM_)

numCategoryTests :: [(Int, NumCategory)]
numCategoryTests =
    [ (28, Perfect)
    , (12, Abundant)
    , (24, Abundant)
    , (15, Deficient)
    ]

--ascNubTests :: [([Int], [Int], [Int])]
--ascNubTests

spec :: Spec
spec =
    describe "numCategoryTests" $
        forM_ numCategoryTests $ \(a, result) ->
            it ("gives " ++ show result ++ " for " ++ show a) $
                numCategory a `shouldBe` result
