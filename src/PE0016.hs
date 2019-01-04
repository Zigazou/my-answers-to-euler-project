{- | Power digit sum
Problem 16

 15
2   = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

                                             1000
What is the sum of the digits of the number 2    ?
-}
module PE0016
( solution

-- For testing purposes
) where

import Data.Char (digitToInt)

-- | Gives the solution to the current problem.
solution :: Int
solution = sum . fmap digitToInt . show $ 2 ^ 1000

-- Functions