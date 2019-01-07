{-# LANGUAGE TupleSections #-}
{- | Non-abundant sums
Problem 23

A perfect number is a number for which the sum of its proper divisors is exactly
equal to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
-}
module PE0023
( solution

-- For testing purposes
, NumCategory (Perfect, Abundant, Deficient)
, numCategory
, abundantNumbersBelow
, allCouples
) where

import Helpers (properDivisors, parallelize)
import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Sort (uniqueSort)

-- | Gives the solution to the current problem.
solution :: Int
solution = sum (nonAbundantSum 28124 :: [Int])

-- Functions
data NumCategory = Perfect | Abundant | Deficient deriving (Show, Eq)
instance NFData NumCategory where rnf = rwhnf

numCategory :: Integral a => a -> NumCategory
numCategory number
    | sumpf > number = Abundant
    | sumpf < number = Deficient
    | otherwise = Perfect
    where sumpf = sum . properDivisors $ number

abundantNumbersBelow :: (NFData a, Integral a) => a -> [a]
abundantNumbersBelow limit = fst <$> filter ((== Abundant) . snd) categories
    where categories = parallelize [(a, numCategory a) | a <- [2..limit-1]]

allCouples :: Integral a => [a] -> [(a,a)]
allCouples [] = []
allCouples [_] = []
allCouples (x:xs) = ((x,) <$> (0:xs)) ++ allCouples xs

ascNub :: Ord a => [a] -> [a] -> [a]
ascNub [] _ = []
ascNub xs [] = xs
ascNub (x:xs) (y:ys)
    | x < y = x:ascNub xs (y:ys)
    | x > y = ascNub (x:xs) ys
    | otherwise = ascNub xs ys

nonAbundantSum :: (NFData a, Ord a, Integral a) => a -> [a]
nonAbundantSum limit = ascNub [2..limit-1]
                     . uniqueSort
                     . fmap (uncurry (+))
                     . allCouples
                     . abundantNumbersBelow
                     $ limit
