module PE0018Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0018

import Control.Monad (forM_)

allPathsTests :: [([[Int]], [[Int]])]
allPathsTests =
    [   ([[1]], [[1]])
    ,   ([[1],[2,3],[4,5,6]], [[1,2,4],[1,2,5],[1,3,5],[1,3,6]])
    ,   ( [[3],[7,4],[2,4,6],[8,5,9,3]]
        , [[3,7,2,8], [3,7,2,5], [3,7,4,5], [3,7,4,9],
          [3,4,4,5], [3,4,4,9], [3,4,6,9], [3,4,6,3]]
        )
    ]

exampleTests :: [(Int, [[Int]])]
exampleTests =
    [   (   23
        ,   [ [3]
            , [7,4]
            , [2,4,6]
            , [8,5,9,3]
            ]
        )
    ,   (1, [[1]])
    ,   (5, [[1],[2,4]])
    ]

maxPathRowTests :: [([Int], [Int], [Int])]
maxPathRowTests =
    [ ([3,4,5], [1,2], [5,7])  
    ]

spec :: Spec
spec = do
    describe "allPaths" $
        forM_ allPathsTests $ \(rows, result) ->
            it ("gives " ++ show result ++ " for " ++ show rows) $
                allPaths rows `shouldBe` result

    describe "maxPath" $
        forM_ exampleTests $ \(result, rows) ->
            it ("gives " ++ show result ++ " for " ++ show rows) $
                maxPath rows `shouldBe` result

    describe "maxPathRow" $
        forM_ maxPathRowTests $ \(a, b, result) ->
            it ("gives " ++ show result ++ " for " ++ show a ++ "," ++ show b) $
                maxPathRow a b `shouldBe` result

    describe "maxPathFast" $
        forM_ exampleTests $ \(result, rows) ->
            it ("gives " ++ show result ++ " for " ++ show rows) $
                maxPathFast rows `shouldBe` result
