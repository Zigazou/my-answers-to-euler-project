module PE0019Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PE0019

import Control.Monad (forM_)

leapTests :: [(Int, LeapYear)]
leapTests =
    [ (1900, NotLeap)
    , (1904, Leap)
    , (2000, Leap)
    ]

firstDayOfYearTests :: [(Int, WeekDay)]
firstDayOfYearTests =
    [ (1900, Monday)
    , (1904, Friday)
    , (1905, Sunday)
    , (2019, Tuesday)
    ]

countFirstDayInMonthTests :: [(WeekDay, Int, Int, Int)]
countFirstDayInMonthTests =
    [ (Monday, 1900, 1900, 2)
    , (Thursday, 1900, 1900, 3)
    , (Sunday, 1900, 1901, 4)
    , (Sunday, 1900, 1902, 5)
    , (Sunday, 1900, 1904, 9)
    ]

spec :: Spec
spec = do
    describe "leap" $
        forM_ leapTests $ \(year, leapOrNot) ->
            it ("gives " ++ show leapOrNot ++ " for " ++ show year) $
                leap year `shouldBe` leapOrNot

    describe "firstDayOfYear" $
        forM_ firstDayOfYearTests $ \(year, weekday) ->
            it ("gives " ++ show weekday ++ " for " ++ show year) $
                firstDayOfYear year `shouldBe` weekday

    describe "countFirstDayInMonth" $
        forM_ countFirstDayInMonthTests $ \(weekday, start, end, result) ->
            it ("gives " ++ show result ++ " for " ++ show weekday ++ " in "
                ++ show start ++ "-" ++ show end) $
                countFirstDayInMonth weekday start end `shouldBe` result
