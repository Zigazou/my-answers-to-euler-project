{- | Largest prime factor
Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}
module PE0003
( problemID
, problemTitle
, solution

-- For testing purposes
, firstPrimes
, genIncrements
, potentials
, divisibleBy
, nextPrime
, nextPrimes
, primes
, primeFactors
) where

-- | ID of the Euler problem.
problemID :: Integer
problemID = 3

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Largest prime factor"

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = last . primeFactors $ 600851475143

-- Functions

-- | First prime numbers are 2, 3, 5, 7, 11, 13 and 17.
firstPrimes :: Integral a => [a]
firstPrimes = [ 2, 3, 5, 7, 11, 13, 17 ]

minus :: Integral a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus (x:xs) (y:ys)
    | x < y = x : minus xs (y:ys)
    | x == y = minus xs (y:ys)
    | otherwise = minus (x:xs) ys

genIncrements :: Integral a => [a] -> [a]
genIncrements numbers = zipWith (-) (tail remains) remains
    where
        maxi = product numbers 
        multiplesOf x = takeWhile (<= maxi) $ iterate (+ x) x
        toRemove = multiplesOf <$> numbers
        remains = foldr (flip minus) [1..maxi + 1] toRemove

-- | Numbers which are not multiples of 2, 3, 5, 7, 11, 13 and 17.
potentials :: Integral a => [a]
potentials = tail $ scanl (+) 1 (cycle (genIncrements firstPrimes))

-- | Check if a number is divisible by any divisor from a list of divisors.
-- Note: the number MUST NOT be found in the divisors list!
divisibleBy :: Integral a
            => a    -- ^ dividend
            -> [a]  -- ^ divisors
            -> Bool -- ^ True if any divisor can divide the dividend
divisibleBy _ [] = False
divisibleBy number divisors =
    any ((0 ==) . mod number) . takeWhile (<= isqrt number) $ divisors
    where isqrt = floor . sqrt . fromIntegral

-- | Given a list of the first n primes, finds the next (n+1) prime number.
nextPrime :: Integral a
          => [a]        -- ^ list of known primes, in ascending order
          -> [a]        -- ^ list of numbers to check for primeness
          -> (a, [a])   -- ^ fst is the next prime number, snd is next numbers
                        --   to check for primeness
nextPrime knownPrimes (p:ps)
    | p `divisibleBy` knownPrimes = nextPrime knownPrimes ps
    | otherwise = (p, ps)

-- | Given a list of the first n primes, gives all following prime numbers.
nextPrimes :: Integral a
           => [a]   -- ^ list of known primes, in ascending order
           -> [a]   -- ^ list of number to check for primeness
           -> [a]   -- ^ list of all following prime numbers
nextPrimes knownPrimes ps =
    newPrime : nextPrimes (knownPrimes ++ [newPrime]) newPotentials
    where (newPrime, newPotentials) = nextPrime knownPrimes ps

-- | List of all prime numbers
primes :: Integral a => [a]
primes = firstPrimes ++ nextPrimes firstPrimes potentials

-- | Gives all prime factors of a number
primeFactors :: Integral a => a -> [a]
primeFactors = primeFactors' primes

-- | Gives all prime factors of a number (without 1)
primeFactors' :: Integral a
              => [a]    -- ^ list of known primes
              -> a      -- ^ starting number (shrinks recursively down to 1)
              -> [a]    -- ^ list of prime factors
primeFactors' _ 1 = []
primeFactors' (p:ps) number
    | mod number p == 0 = p : primeFactors' (p:ps) (div number p)
    | otherwise = primeFactors' ps number
