{- | Number letter counts
Problem 17

If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with British
usage.
-}
module PE0017
( problemID
, problemTitle
, solution

-- For testing purposes
, Flavour (British, American)
, groupDigits
, numberToEnglish
, countLetters
) where

import Data.Char (digitToInt, isLetter)
import Data.Maybe (fromMaybe)
import Data.List (find)

-- | ID of the Euler problem.
problemID :: Integer
problemID = 17

-- | Title of the Euler problem.
problemTitle :: String
problemTitle = "Number letter counts"

-- | Gives the solution to the current problem.
solution :: Int
solution = sum . fmap (countLetters . numberToEnglish British) $ [1..1000]

-- Functions
data Flavour = British | American deriving (Show, Eq)

units :: Flavour -> [String]
units _ =
    [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight"
    , "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
    , "sixteen", "seventeen", "eighteen", "nineteen"
    ]

tenUnits :: Flavour -> [String]
tenUnits _ =
    [ "", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy"
    , "eighty", "ninety"
    ]

-- https://simple.wikipedia.org/wiki/Names_for_large_numbers
thousandUnits :: Flavour -> [String]
thousandUnits British =
    [ "", "thousand", "million", "milliard", "billion", "billiard", "trillion"
    , "trilliard", "quadrillion", "quadrilliard", "quintillion", "quintilliard"
    , "sextillion", "sextilliard", "septillion", "septilliard", "octillion"
    , "octilliard", "nonillion", "nonilliard", "decillion", "decilliard"
    , "undecillion", "undecilliard", "duodecillion", "duodecilliard"
    , "tredecillion", "tredecilliard", "quattuordecillion", "quattuordecilliard"
    , "quindecillion", "quindecilliard", "sexdecillion", "sexdecilliard"
    , "septendecillion", "septendecilliard"
    ]
thousandUnits American = 
    [ "", "thousand", "million", "billion", "trillion", "quadrillion"
    , "quintillion", "sextillion", "septillion", "octillion", "nonillion"
    , "decillion", "undecillion", "duodecillion", "tredecillion"
    , "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion"
    , "octodecillion", "novemdecillion", "vigintillion", "unvigintillion"
    , "duovigintillion", "trevigintillion", "quattuorvigintillion"
    , "quinvigintillion", "sexvigintillion", "septenvigintillion"
    , "octovigintillion", "novemvigintillion", "trigintillion"
    , "untrigintillion", "duotrigintillion", "tretrigintillion"
    , "quattuortrigintillion"
    ]

groupDigits :: Integral a => a -> [(Int, Int)]
groupDigits = filter ((/= 0) . snd) . reverse . groupDigits' widthPowers
    where
        widthPowers :: [(Int, Int)]
        widthPowers = zip ([2, 1] ++ repeat 3) ([0, 2] ++ [3,6..])

        groupDigits' :: Integral a => [(Int, Int)] -> a -> [(Int, Int)]
        groupDigits' _ 0 = []
        groupDigits' ((width, power):nexts) value =
              (power, fromIntegral (mod value (10 ^ width)))
            : groupDigits' nexts (div value (10 ^ width))

numberToEnglish :: (Integral a, Show a) => Flavour -> a -> String
numberToEnglish flavour 0 = head (units flavour)
numberToEnglish flavour number = grps2eng (groupDigits number)
    where
        separator :: Flavour -> String
        separator British = " and "
        separator American = " "

        grp2eng :: (Int, Int) -> String
        grp2eng (_, 0) = ""
        grp2eng (0, tens)
            | tens < 20 = units flavour !! tens
            | tens < 100 = tenUnits flavour !! mod (div tens 10) 10
                     ++ if mod tens 10 == 0
                            then ""
                            else "-" ++ (units flavour !! mod tens 10)
            | mod tens 100 == 0 = grp2eng (2, div tens 100)
            | otherwise = grp2eng (2, div tens 100)
                       ++ separator flavour
                       ++ grp2eng (0, mod tens 100)
        grp2eng (2, value) = (units flavour !! value) ++ " hundred"
        grp2eng (n, value) = grp2eng (0, mod value 1000)
                          ++ " "
                          ++ (thousandUnits flavour !! div n 3)

        grps2eng :: [(Int, Int)] -> String
        grps2eng [(0, value)] = grp2eng (0, value)
        grps2eng [(2, h), (0, u)] = grp2eng (0, h * 100 + u)
        grps2eng [(n, t), (2, h), (0, u)] =
            grp2eng (n, t) ++ " " ++ grps2eng [(2, h), (0, u)]
        grps2eng [(n, t), (0, u)] =
            grp2eng (n, t) ++ separator flavour ++ grp2eng (0, u)
        grps2eng [(n, t)] = grp2eng (n, t)
        grps2eng ((n, t):ns) = grp2eng (n, t) ++ " " ++ grps2eng ns

countLetters :: String -> Int
countLetters = length . filter isLetter