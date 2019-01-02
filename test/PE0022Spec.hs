module PE0022Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0022

import Control.Monad (forM_)

alphabeticalValueTests :: [(String, Int)]
alphabeticalValueTests =
    [ ("COLIN", 53)
    , ("AAAAA", 5)
    ]

nameScoreTests :: [((Int, String), Int)]
nameScoreTests =
    [ ((938, "COLIN"), 49714)
    ]

spec :: Spec
spec = do
    describe "alphabeticalValueTests" $
        forM_ alphabeticalValueTests $ \(a, result) ->
            it ("gives " ++ show result ++ " for " ++ show a) $
                alphabeticalValue a `shouldBe` result

    describe "nameScoreTests" $
        forM_ nameScoreTests $ \(a, result) ->
            it ("gives " ++ show result ++ " for " ++ show a) $
                nameScore a `shouldBe` result
