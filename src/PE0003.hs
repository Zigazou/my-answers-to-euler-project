{- | Largest prime factor
Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
module PE0003
( problemID
, problemTitle
, solution

-- For testing purposes
) where

import Helpers (primeFactors)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 3

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Largest prime factor"

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = last . primeFactors $ 600851475143
