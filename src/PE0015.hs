{- | Lattice paths
Problem 15

Starting in the top left corner of a 2×2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

    ____   __      __      
        |    |__     |     |____   |__      |
        |       |    |__\       |     |__\  |____\
        V       V       /       V        /       /

How many such routes are there through a 20×20 grid?
-}
module PE0015
( solution

-- For testing purposes
, combinations
) where

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = combinations 20

-- Functions
combinations :: Integral a => a -> a
combinations n = product [1..n*2] `div` (product [1..n] ^ 2)
