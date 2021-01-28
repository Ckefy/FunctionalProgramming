module Integrate where

import System.Random
import Text.Printf
import Data.List

import qualified Control.Concurrent.Thread as T
import Control.Exception.Base (mask_)
import Control.Monad (forM_, forM)
import Control.Concurrent.STM (TVar, atomically, STM, readTVar, newTVar, writeTVar, readTVarIO)
import Control.Monad.Par


-- | Uniform random generator used
-- | Monte => Integral [a, b] (f(x)) = (b - a) / N * sum[1..N](f(rand[i]))
integrate :: (Double -> Double) -> Double -> Double -> Int -> IO Double
integrate f a b n = do
   randGen <- newStdGen
   let randArr = take n (randomRs (a, b) randGen)
   let sum = foldl' (+) 0 (map f randArr)
   return ((b - a) / fromIntegral n * sum)

implF :: (Double -> Double) -> Double -> IO Double
implF f x = return (f x)

integrateParallel :: (Double -> Double) -> Double -> Double -> Int -> IO Double
integrateParallel f a b n = do
   randGen <- newStdGen
   let randArr = take n (randomRs (a, b) randGen)
   let sum = runPar (parMapReduceRange (InclusiveRange 1 n) (return . f . fromIntegral) 
                                          (\first second -> return (first + second)) 0)
   return ((b - a) / fromIntegral n * sum)

testMain :: IO()
testMain = do
   let f x = cos x--1 / tan (x * x) - cos x
   let a = 2
   let b = 5
   let n = 1200000 :: Int
   ansNaive <- integrateParallel f a b n
   printf "%.12f" ansNaive
   putStrLn ""
