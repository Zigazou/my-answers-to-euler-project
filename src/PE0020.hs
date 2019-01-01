{- | Factorial digit sum
Problem 20

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
-}
module PE0020
( problemID
, problemTitle
, solution

-- For testing purposes
) where

import Data.Char (digitToInt)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 20

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Factorial digit sum"

-- | Gives the solution to the current problem.
solution :: Integer
solution = fromIntegral . sum . fmap digitToInt . show . product $ [1..100]

-- Functions
