module Main where

import HashTable
import qualified Control.Concurrent.Thread as T
import Criterion.Main
import System.Random
import Control.Monad (forM_)
import qualified GeometryParallel as Parallel
import qualified GeometryNaive as Naive
import Integrate

main :: IO ()
main = defaultMain [bgroup "Benchmarking Perimeter function" [benchPerimeterParallel, benchPerimeterNaive]
                  , bgroup "Benchmarking Area function" [benchAreaParallel, benchAreaNaive]
                  , bgroup "Benchmarking Integrate function" [benchIntegrateParallel, benchIntegrateNaive]
                  , benchCHT]

--FIRST TASK
-- | True means parallel impl, False - naive impl
gen :: Bool -> ([Parallel.Point], [Naive.Point])
gen flag = if flag then
             (zipWith Parallel.Point [1 .. 10 ^ (7 :: Int)] [1 .. 10 ^ (7 :: Int)] ++ 
                       [Parallel.Point 100500 y | y <- [0..500000]], undefined)
           else
             (undefined, zipWith Naive.Point [1 .. 10 ^ (6 :: Int)] [1 .. 10 ^ (6 :: Int)] ++
                                  [Naive.Point x 100500 | x <- [0..500000]])

benchPerimeterParallel :: Benchmark
benchPerimeterParallel = bench "Perimeter Parallel impl" $ nf Parallel.perimeter (fst(gen True))

benchPerimeterNaive :: Benchmark
benchPerimeterNaive = bench "Perimeter Naive impl" $ nf Naive.perimeter (snd(gen False))

benchAreaParallel :: Benchmark
benchAreaParallel = bench "Area Parallel impl" $ nf Parallel.doubleArea (fst(gen True))

benchAreaNaive :: Benchmark
benchAreaNaive = bench "Area Naive impl" $ nf Naive.doubleArea (snd(gen False))

--SECOND TASK

benchIntegrateParallel :: Benchmark
benchIntegrateParallel = bench "Monte-Carlo method parallel impl (10^6 operations)" $ nfIO (genIntegrate True)

benchIntegrateNaive :: Benchmark
benchIntegrateNaive = bench "Monte-Carlo method naive impl (10^6 operations)" $ nfIO (genIntegrate False)

genIntegrate :: Bool -> IO()
genIntegrate flag = do
      let f x = 1 / tan (x * x) - cos x
      let a = 2
      let b = 5
      let n = 1200000 :: Int
      if flag then
        integrateParallel f a b n
      else
         integrate f a b n
      return()

--THIRD TASK

benchCHT :: Benchmark
benchCHT = bench "Benchmarking Hashtable with 3 threads (more than 10^5 operations)" $ nfIO genCHT

genCHT :: IO ()
genCHT = do
  hashTable <- newCHT
  forM_ [[(1 :: Int)..35000], [(1 :: Int)..35000], [(1 :: Int)..35000]] $ \curList -> do
          (_, res') <- T.forkIO $
            forM_ curList $ \x -> 
              putCHT ("multiT" ++ show x) ((100500 :: Int) + x) hashTable
          res <- res'
          T.result res
  forM_ [[(1 :: Int)..35000], [(1 :: Int)..35000], [(1 :: Int)..35000]] $ \curList -> do
          (_, res') <- T.forkIO $
            forM_ curList $ \x -> 
              getCHT ("multiT" ++ show x) hashTable
          res <- res'
          T.result res