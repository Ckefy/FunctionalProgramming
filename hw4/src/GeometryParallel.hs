{-# LANGUAGE BangPatterns #-}
module GeometryParallel where

import Control.DeepSeq (NFData, rnf, ($!!))

data Point = Point !Int !Int
   deriving (Show, Eq)

instance NFData Point where
   rnf (Point x y) = seq (rnf x) (rnf y)

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
-- | Args: func accumulator others (the last one is the copy of first!)
makeCalc :: (NFData a, Num a) => (Point -> Point -> a) -> a -> [Point] -> a
makeCalc func !accum [] = accum
makeCalc func !accum [last] = accum
makeCalc func !accum (cur:others) = (makeCalc func $!! ((accum +) $!! func cur (head others))) others

-- | Calculate pythagoras distance between two points
dist :: Point -> Point -> Double
dist first second = sqrt (fromIntegral (scalarProduct (minus first second) (minus first second)))

-- | Given polygon without self-crossing, calculate perimeter
perimeter :: [Point] -> Double
perimeter [] = 0
perimeter l@(cur:others) = makeCalc dist 0 (l ++ [cur])

-- | Given polygon without self-crossing, calculate 2x square
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea l@(cur:others) = abs (makeCalc crossProduct 0 (l ++ [cur]))




