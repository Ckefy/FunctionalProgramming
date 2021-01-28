module TestHashTable where

import HashTable
import Control.Monad (forM_, forM)
import qualified Control.Concurrent.Thread as T
import Control.Exception.Base
import Control.Concurrent
import Test.Tasty
import Test.Tasty.HUnit

allFunctionsTests :: Assertion
allFunctionsTests = do
    hashTable1 <- newCHT
    size1 <- sizeCHT hashTable1
    size1 @?= 0
    valueNothing <- getCHT "test" hashTable1
    valueNothing @?= Nothing
    putCHT "test" (1 :: Int) hashTable1
    size2 <- sizeCHT hashTable1
    size2 @?= 1
    valueJust <- getCHT "test" hashTable1
    valueJust @?= Just 1
    putCHT "test1" (1 :: Int) hashTable1
    putCHT "test2" (2 :: Int) hashTable1
    putCHT "test3" (1 :: Int) hashTable1
    size3 <- sizeCHT hashTable1
    size3 @?= 4
    valueJust1 <- getCHT "test1" hashTable1
    valueJust1 @?= Just 1
    valueJust2 <- getCHT "test2" hashTable1
    valueJust2 @?= Just 2

multithreadQueriesTests :: Assertion
multithreadQueriesTests = do
    hashTable1 <- newCHT
    forM_ [1 :: Int, 2, 3, 4, 5] $ \x -> do
        (_, res') <- T.forkIO $
          putCHT ("multiT" ++ show x) ((100500 :: Int) + x) hashTable1
        res <- res'
        T.result res
    ans <- forM [1 :: Int, 2, 3, 4, 5] $ \x -> do
                (_, res') <- T.forkIO $
                   getCHT ("multiT" ++ show x) hashTable1
                res <- res'
                T.result res
    head ans @?= Just (100501 :: Int)
    (ans !! 1) @?= Just 100502
    (ans !! 2) @?= Just 100503
    (ans !! 3) @?= Just 100504
    (ans !! 4) @?= Just 100505

myHandler :: AsyncException -> IO ()
myHandler err = putStr ""

asyncTests :: Assertion
asyncTests = do
      hashTable1 <- newCHT
      (threadID, res') <- T.forkIO $ do
        threadDelay 50
        forM_ [1..50] $ \x ->
           putCHT ("multiT" ++ show x) ((100500 :: Int) + x) hashTable1
      threadDelay 50
      throwTo threadID ThreadKilled
      ans <- res'
      --now handle it
      handle myHandler (T.result ans)
      ans2 <- forM [(1 :: Int)..50] (\x ->
                 getCHT ("multiT" ++ show x) hashTable1)
      let pairs = zip ans2 [(1 :: Int)..50]
      forM_ pairs (\pair -> case pair of
                     (Just smth, index) -> pair @?= (Just ((100500 :: Int) + index), index)
                     (Nothing, _) -> True @?= True
       )

testHashTable :: TestTree
testHashTable =
    testGroup
    "Test module HashTable (3st task)"
    [testCase "Test all functions (newCHT, sizeCHT, putCHT, getCHT) of hash table" allFunctionsTests
   , testCase "Test correctness of multithread queries" multithreadQueriesTests
   , testCase "Test resistance of hash map to async exceptions" asyncTests
   ]

