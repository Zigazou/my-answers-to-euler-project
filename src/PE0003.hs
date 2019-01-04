{- | Largest prime factor
Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
module PE0003
( solution

-- For testing purposes
) where

import Helpers (primeFactors)

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = last . primeFactors $ 600851475143
