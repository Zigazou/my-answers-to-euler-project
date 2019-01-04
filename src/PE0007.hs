{- | 10001st prime
Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10001st prime number?
-}
module PE0007
( solution

-- For testing purposes
) where

import Helpers (primes)

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = primes !! 10000

-- Functions
