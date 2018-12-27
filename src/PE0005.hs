{- | Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to
10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
-}
module PE0005
( problemID
, problemTitle
, solution

-- For testing purposes
, divisibleByAll
, smallestMultiple
) where

import Data.List (find, (\\))
import Data.Maybe (fromMaybe)
import PE0003 (primes)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 5

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Smallest multiple"

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = smallestMultiple [1..20]

-- Functions

-- | Check if a number is divisible by all divisors from a list of divisors.
divisibleByAll :: Integral a
               => a    -- ^ dividend
               -> [a]  -- ^ divisors
               -> Bool -- ^ True if all divisors can divide the dividend
divisibleByAll number = all ((0 ==) . mod number)

-- | Find the smallest multiple of a list.
smallestMultiple :: (Integral a, Ord a) => [a] -> a
smallestMultiple numbers = fromMaybe 0
                         $ find (\x -> divisibleByAll x notPrimes) multiples
    where
        -- Find the maximum of the list
        maxi = maximum numbers

        -- Separate prime numbers from non prime numbers in the number list
        thePrimes = takeWhile (<= maxi) primes
        notPrimes = numbers \\ thePrimes

        -- The smallest multiple is a multiple of the product of the prime
        -- numbers from the list.
        multiple = product thePrimes
        multiples = iterate (+ multiple) multiple
