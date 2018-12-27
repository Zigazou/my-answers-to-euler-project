{-# LANGUAGE TupleSections #-}

{- | Largest palindrome product
Problem 4

A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}
module PE0004
( problemID
, problemTitle
, solution

-- For testing purposes
, isPalindromic
, fixedDigits
, allCouples
, firstPalindrom
, biggestPalindrom
, pretty
, CoupleProduct (CoupleProduct, cpProduct, cpNumberA, cpNumberB)
, makeCoupleProduct
) where

import Control.Monad (liftM2)
import Data.List (find)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 4

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Largest palindrome product"

-- | Gives the solution to the current problem.
solution :: (Integral a, Show a) => Maybe a
solution = cpProduct <$> biggestPalindrom 3

-- Functions
data CoupleProduct a = CoupleProduct
    { cpProduct :: a
    , cpNumberA :: a
    , cpNumberB :: a
    } deriving (Show, Eq)

makeCoupleProduct (x, y) = CoupleProduct (x * y) x y

instance (Integral a, Show a, Ord a, Eq a) => Ord (CoupleProduct a) where
    (CoupleProduct m1 _ _) `compare` (CoupleProduct m2 _ _) = m1 `compare` m2

pretty :: (Integral a, Show a) => CoupleProduct a -> String
pretty (CoupleProduct p a1 a2) = show p ++ "=" ++ show a1 ++ "×" ++ show a2

isPalindromic :: Show a => a -> Bool
isPalindromic = liftM2 (==) show (reverse . show)

fixedDigits :: Integral a => a -> [a]
fixedDigits digitNumber = [startNumber, startNumber - 1 .. endNumber]
    where startNumber = 10 ^ digitNumber - 1
          endNumber = 10 ^ (digitNumber - 1)

allCouples :: Num a => [a] -> [[(a, a)]]
allCouples [] = []
allCouples numbers@(n:ns) = ((n,) <$> numbers) : allCouples ns

firstPalindrom :: (Integral a, Show a)
               => [CoupleProduct a]
               -> Maybe (CoupleProduct a)
firstPalindrom = find palindromicCouple
    where palindromicCouple cp = isPalindromic (cpProduct cp)

biggestPalindrom :: (Integral a, Show a) => a -> Maybe (CoupleProduct a)
biggestPalindrom digits = maximum palindroms
    where
        couples = allCouples $ fixedDigits digits
        coupleProducts = fmap makeCoupleProduct <$> couples
        palindroms = firstPalindrom <$> coupleProducts
