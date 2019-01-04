{- | Counting Sundays
Problem 19

You are given the following information, but you may prefer to do some research
for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century
    unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?
-}
module PE0019
( solution

-- For testing purposes
, LeapYear (Leap, NotLeap)
, WeekDay (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
, leap
, leapYears
, daysPerMonth
, daysPerYear
, daysPerMonthPerYear
, daysPerYearPerYear
, firstDayOfYear
, countFirstDayInMonth
, monthFirstDays
) where

-- | Gives the solution to the current problem.
solution :: Integral a => a
solution = countFirstDayInMonth Sunday 1901 2000

-- Functions
data LeapYear = Leap | NotLeap deriving (Show, Eq)

data WeekDay =
    Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq)

weekDayOffset :: Integral a => WeekDay -> a
weekDayOffset Monday = 0
weekDayOffset Tuesday = -1
weekDayOffset Wednesday = -2
weekDayOffset Thursday = -3
weekDayOffset Friday = -4
weekDayOffset Saturday = -5
weekDayOffset Sunday = -6

offsetWeekDay :: Integral a => a -> WeekDay
offsetWeekDay 0 = Monday
offsetWeekDay 1 = Tuesday
offsetWeekDay 2 = Wednesday
offsetWeekDay 3 = Thursday
offsetWeekDay 4 = Friday
offsetWeekDay 5 = Saturday
offsetWeekDay 6 = Sunday
offsetWeekDay (-1) = Sunday
offsetWeekDay (-2) = Saturday
offsetWeekDay (-3) = Friday
offsetWeekDay (-4) = Thursday
offsetWeekDay (-5) = Wednesday
offsetWeekDay (-6) = Tuesday
offsetWeekDay _ = undefined

leap :: Integral a => a -> LeapYear
leap year
    | mod year 4 /= 0 = NotLeap
    | mod year 400 == 0 = Leap
    | mod year 100 == 0 = NotLeap
    | otherwise = Leap

leapYears :: Integral a => a -> a -> [LeapYear]
leapYears start end = leap <$> [start..end]

daysPerMonth :: Integral a => LeapYear -> [a]
daysPerMonth NotLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
daysPerMonth Leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

daysPerYear :: Integral a => LeapYear -> a
daysPerYear NotLeap = 365
daysPerYear Leap = 366

daysPerMonthPerYear :: Integral a => [LeapYear] -> [[a]]
daysPerMonthPerYear = fmap daysPerMonth

daysPerYearPerYear :: Integral a => [LeapYear] -> [a]
daysPerYearPerYear = fmap daysPerYear

firstDayOfYear :: Integral a => a -> WeekDay
firstDayOfYear = offsetWeekDay
               . (`mod` 7)
               . sum
               . daysPerYearPerYear
               . leapYears 1900
               . (+) (-1)

countFirstDayInMonth :: Integral a => WeekDay -> a -> a -> a
countFirstDayInMonth weekday start end =
    fromIntegral . length . filter (== weekday) $ monthFirstDays start end

monthFirstDays :: Integral a => a -> a -> [WeekDay]
monthFirstDays start end = fmap (offsetWeekDay . (+ offset) . (`mod` 7))
                         $ scanl (+) 0
                         $ init
                         $ concat
                         $ daysPerMonthPerYear
                         $ leapYears start end
    where offset = weekDayOffset . firstDayOfYear $ start
