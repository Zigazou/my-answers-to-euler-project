{- | Summation of primes
Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}
module PE0010
( solution

-- For testing purposes
) where

import Helpers (primes, parallelize)

-- | Gives the solution to the current problem.
solution :: Int
solution = sum . takeWhile (< 2000000) $ primes

-- Functions
