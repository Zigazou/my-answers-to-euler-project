{- | Highly divisible triangular number
Problem 12

The sequence of triangle numbers is generated by adding the natural numbers.
So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred
divisors?
-}
module PE0012
( problemID
, problemTitle
, solution

-- For testing purposes
, triangleNumber
, triangleNumbers
, factors
, numberOfDivisors
, combinations
, findMinDivisors
) where

import Data.List (find, nub)
import Helpers (primes)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 12

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Highly divisible triangular number"

-- | Gives the solution to the current problem.
solution :: Integral a => Maybe a
solution = findMinDivisors 500

-- Functions

triangleNumber :: Integral a => a -> a
triangleNumber n = n * (n + 1) `div` 2

triangleNumbers :: Integral a => [a]
triangleNumbers = triangleNumber <$> [1..]

combinations :: Integral a => [a] -> [[a]]
combinations [] = []
combinations (x:xs) = [x]: ([x:ys | ys <- combinations xs] ++ combinations xs)

numberOfDivisors :: Integral a => a -> a
numberOfDivisors =
    (+ 1) . fromIntegral . length . nub . (product <$>) . combinations . factors

factors :: Integral a => a -> [a]
factors = factors' primes
    where factors' divisors@(d:ds) x
            | x == 1 = []
            | x `mod` d == 0 = d : factors' divisors (x `div` d)
            | otherwise = factors' ds x

findMinDivisors :: Integral a => a -> Maybe a
findMinDivisors mini = find ((> mini) . numberOfDivisors) triangleNumbers
