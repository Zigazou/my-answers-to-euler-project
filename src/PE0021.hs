{- | Amicable numbers
Problem 21

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}
module PE0021
( solution

-- For testing purposes
, amicableNumbers
, amicableNumber
, amicableNumbersBelow
) where

import Helpers (properDivisors)

-- | Gives the solution to the current problem.
solution :: Int
solution = sum . amicableNumbersBelow $ 10000

-- Functions
sumOfProperDivisors :: Integral a => a -> a
sumOfProperDivisors = sum . properDivisors

amicableNumbers :: Integral a => a -> a -> Bool
amicableNumbers a b = (a /= b) && (d a == b) && (d b == a)
    where d = sumOfProperDivisors

amicableNumber :: Integral a => a -> Bool
amicableNumber a = (da /= a) && (sumOfProperDivisors da == a)
    where da = sumOfProperDivisors a

amicableNumbersBelow :: Integral a => a -> [a]
amicableNumbersBelow top = filter amicableNumber [2..top-1]
