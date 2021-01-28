{-# LANGUAGE InstanceSigs #-}
--Task 1.1
module Days where

data Day = Monday
          | Tuesday
          | Wednesday
          | Thursday
          | Friday
          | Saturday
          | Sunday
          deriving (Show)

-- | Transform Day to integer number
dayToNumber :: Day -> Int
dayToNumber Monday = 0
dayToNumber Tuesday = 1
dayToNumber Wednesday = 2
dayToNumber Thursday = 3
dayToNumber Friday = 4
dayToNumber Saturday = 5
dayToNumber Sunday = 6


instance Eq Day where
    (==) :: Day -> Day -> Bool
    day == anotherDay = dayToNumber day == dayToNumber anotherDay

-- | Transform integer number to Day (backward to dayToNumber function)
numberToDay :: Int -> Day
numberToDay 0 = Monday
numberToDay 1 = Tuesday
numberToDay 2 = Wednesday
numberToDay 3 = Thursday
numberToDay 4 = Friday
numberToDay 5 = Saturday
numberToDay 6 = Sunday

-- | Return the day, that comes after income
nextDay :: Day -> Day
nextDay day = numberToDay ((dayToNumber day + 1) `mod` 7)

-- | Take the day and integer number n, return day that comes after n days
afterDays :: Day -> Int -> Day
afterDays day number = numberToDay ((dayToNumber day + number) `mod` 7)

-- | Check if income Day is Saturday or Sunday
isWeekend :: Day -> Bool
isWeekend day = dayToNumber day > 4

-- | Check when will be the next Friday
daysToParty :: Day -> Int
daysToParty day = (7 + dayToNumber Friday - dayToNumber day) `mod` 7