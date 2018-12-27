{-# LANGUAGE DeriveFoldable #-}
{- | Special Pythagorean triplet
Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a² + b² = c²

For example, 3² + 4² = 9 + 16 = 25 = 5².

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}
module PE0009
( problemID
, problemTitle
, solution

-- For testing purposes
, Triplet (Triplet)
, isPythagorean
, allTripletsSumEquals
, triplet1000
) where

import Data.List (find)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 9

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Special Pythagorean triplet"

-- | Gives the solution to the current problem.
solution :: Maybe Integer
solution = product <$> triplet1000

-- Functions
data Triplet a = Triplet
    { tA :: a
    , tB :: a
    , tC :: a
    } deriving (Show, Eq, Foldable)

isPythagorean :: (Integral a, Ord a) => Triplet a -> Bool
isPythagorean (Triplet a b c) = a < b && b < c && a * a + b * b == c * c

allTripletsSumEquals :: Integral a => a -> [Triplet a]
allTripletsSumEquals value = do
    a <- [1 .. div value 3]
    b <- [a .. div (value - a) 2 + 1]
    let c = value - a - b
    return (Triplet a b c)

triplet1000 :: Integral a => Maybe (Triplet a)
triplet1000 = find isPythagorean (allTripletsSumEquals 1000)
