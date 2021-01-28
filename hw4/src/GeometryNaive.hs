module GeometryNaive where

import Control.DeepSeq (NFData, rnf)

data Point = Point Int Int
   deriving (Show)

-- | Addition of two points
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Subtraction of two points
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar multiplying of two points
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = (x1 * x2) + (y1 * y2)

-- | Pseudo-scalar multiplying of two points
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = (x1 * y2) - (x2 * y1)

-- | Make calculation of given func to the whole list and summarize it
-- | Make it naive, with tail recursion - must be very slow
makeCalc :: (Num a) => (Point -> Point -> a) -> [Point] -> a
makeCalc func [] = 0
makeCalc func [last] = 0
makeCalc func (cur:others) = func cur (head others) + makeCalc func others

-- | Calculate pythagoras distance between two points
dist :: Point -> Point -> Double
dist first second = sqrt (fromIntegral (scalarProduct (minus first second) (minus first second)))

-- | Given polygon without self-crossing, calculate perimeter
perimeter :: [Point] -> Double
perimeter [] = 0
perimeter l@(cur:others) = makeCalc dist (l ++ [cur])

-- | Given polygon without self-crossing, calculate 2x square
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea l@(cur:others) = abs (makeCalc crossProduct (l ++ [cur]))




