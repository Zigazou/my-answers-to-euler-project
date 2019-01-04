{- | Sum square difference
Problem 6

The sum of the squares of the first ten natural numbers is,
1² + 2² + ... + 10² = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)² = 55² = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
-}
module PE0006
( solution

-- For testing purposes
, sumOfSquares
, squareOfSum
, sumSquareDifference
) where

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = sumSquareDifference [1..100]

-- Functions
sumOfSquares :: Integral a => [a] -> a
sumOfSquares = sum . fmap (^ 2)

squareOfSum :: Integral a => [a] -> a
squareOfSum = (^ 2) . sum

sumSquareDifference :: Integral a => [a] -> a
sumSquareDifference ls = squareOfSum ls - sumOfSquares ls
