{- | Power digit sum
Problem 16

 15
2   = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

                                             1000
What is the sum of the digits of the number 2    ?
-}
module PE0016
( problemID
, problemTitle
, solution

-- For testing purposes
) where

import Data.Char (digitToInt)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 16

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Power digit sum"

-- | Gives the solution to the current problem.
solution :: Int
solution = sum . fmap digitToInt . show $ 2 ^ 1000

-- Functions