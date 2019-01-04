{- | Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}
module PE0001
( solution

-- For testing purposes
, multiplesOf
, union
, sumMultiplesOf
) where

-- | Gives the solution to the current problem.
solution :: (Ord a, Num a, Show a) => a
solution = sumMultiplesOf 3 5 1000

-- Functions

-- | Gives all multiples of a number (infinite list in ascending order).
multiplesOf :: Num a => a -> [a]
multiplesOf number = iterate (+ number) number

{- | Merge two lists in ascending order, removing duplicate values.
The lists must not have themselves duplicate values!
-}
union :: Ord a => [a] -> [a] -> [a]
union lx [] = lx
union [] ly = ly
union lx@(x:xs) ly@(y:ys)
    | x == y = x : union xs ys
    | x < y = x : union xs ly
    | otherwise = y : union lx ys

-- | Calculate the sum of the multiples of two numbers below a maximum.
sumMultiplesOf :: (Ord a, Num a)
               => a -- ^ first number
               -> a -- ^ second number
               -> a -- ^ maximum value (not included)
               -> a -- ^ sum of the multiples of the two numbers
sumMultiplesOf x y maximum = sum
                           $ takeWhile (< maximum)
                           $ union (multiplesOf x) (multiplesOf y)
