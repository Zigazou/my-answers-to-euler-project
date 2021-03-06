{- | Even Fibonacci numbers
Problem 2

Each new term in the Fibonacci sequence is generated by adding the previous two
terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed
four million, find the sum of the even-valued terms.
-}
module PE0002
( solution

-- For testing purposes
, fibonacci
, sumFibonacci
) where

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = sumFibonacci (<= 4000000) even

-- Functions

-- | Gives the fibonacci sequence (infinite list)
fibonacci :: Num a => [a]
fibonacci = fibonacci' 1 2
    where fibonacci' x y = x : y : fibonacci' (x + y) (x + y + y)

-- | Given limiter and filter functions, calculates the sum of the numbers.
sumFibonacci :: (Ord a, Num a)
             => (a -> Bool) -- ^ limiter function
             -> (a -> Bool) -- ^ filter function
             -> a           -- ^ result
sumFibonacci limiter keepOnly =
    sum $ takeWhile limiter $ filter keepOnly fibonacci
