{- | Longest Collatz sequence
Problem 14

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}
module PE0014
( solution

-- For testing purposes
, collatzSequence
, nextCollatzNumber
, collatzLength
, collatzLengthS
) where

import Control.Monad.State.Lazy (StateT, get, put, runState)
import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Gives the solution to the current problem.
solution :: Int
solution = fst $ maximumBy (comparing snd) (collatzLength [2..999999])

-- Functions
collatzSequence :: Integral a => a -> [a]
collatzSequence 1 = [1]
collatzSequence x = x : collatzSequence (nextCollatzNumber x)

nextCollatzNumber :: Integral a => a -> a
nextCollatzNumber n
    | even n = div n 2
    | otherwise = 3 * n + 1

collatzLength :: Integral a => [a] -> [(a, a)]
collatzLength numbers = collatzLength' numbers (Map.singleton 1 1)
    where
        collatzLength' [] _ = []
        collatzLength' (n:ns) cache = (n, len):collatzLength' ns cache'
            where (len, cache') = runState (collatzLengthS n) cache

collatzLengthS :: (Ord a, Integral a, Monad m) => a -> StateT (Map.Map a a) m a
collatzLengthS number = do
    cache <- get
    case Map.lookup number cache of
        Just len -> return len
        Nothing -> do
            len <- collatzLengthS (nextCollatzNumber number)
            put (Map.insert number (len + 1) cache)
            return (len + 1)
