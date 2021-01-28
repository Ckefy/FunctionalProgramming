module HashTable where

import Data.Vector as Vector
import Data.Bits
import Data.Hashable (Hashable, hash)
import Control.Exception.Base (mask_)
import Control.Monad (forM_, forM)
import Control.Concurrent.STM (TVar, atomically, STM, readTVar, newTVar, writeTVar, readTVarIO)

-- | Tuples with keys, which have equal hash
type Elem k v = TVar [(k, v)]

-- | Use Vector for better performing
-- | First argument is Vector which contain Elems, each of them is array of
-- | tuple with equal hash
-- | Second - size of Vector
data ConcurrentHashTable k v = ConcurrentHashTable (TVar (Vector.Vector (Elem k v))) (TVar Int)

-- | As in standart HM - index = hashCode(key) & (n-1).
getByKey :: (Hashable k) => k -> Vector.Vector (Elem k v) -> STM (Elem k v)
getByKey key vect = do
   let n = Vector.length vect
   let hashCode = hash key
   let index = (.&.) hashCode (n - 1)
   return (vect ! index)

-- | Add tuple to the arr
inserting :: (Hashable k) => Vector.Vector (Elem k v) -> (k, v) -> STM()
inserting vect (key, value) = do
   elem <- getByKey key vect
   arr <- readTVar elem
   writeTVar elem ((key, value) : arr)

-- | Size of new vector is equal to 8 (hyperparameter)
newCHT :: IO (ConcurrentHashTable k v)
newCHT = mask_ (atomically (do
           size <- newTVar 0
           let vectWrapped = Vector.replicateM 8 (newTVar [])
           vect <- vectWrapped >>= newTVar
           return (ConcurrentHashTable vect size)
         ))

-- | Get Just value if such key exist, Nothing otherwise     
getCHT :: (Eq k, Hashable k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable vect size) = mask_ (atomically (do
            openVector <- readTVar vect
            elem <- getByKey key openVector
            arr <- readTVar elem
            return (lookup key arr)
         ))
         
-- | If there weren't such key then we maybe need to resize and add another Elem
-- | with this tuple
-- | Otherwise just rewrite value of existing key 
putCHT :: (Eq k, Hashable k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable vect size) = mask_ (atomically (do
            openVector <- readTVar vect
            elem <- getByKey key openVector
            arr <- readTVar elem
            openSize <- readTVar size
            case lookup key arr of
               Nothing -> do
                  writeTVar size (openSize + 1)
                  if fromIntegral openSize / fromIntegral (Vector.length openVector) < 0.5 then
                      writeTVar elem ((key, value) : arr)
                  else do
                    newVect <- Vector.replicateM (2 * (Vector.length openVector)) (newTVar [])  
                    old <- Control.Monad.forM (Vector.toList openVector) readTVar
                    let new = (key, value) : Prelude.concat old
                    Control.Monad.forM_ new (inserting newVect)
                    writeTVar vect newVect
               Just value -> writeTVar elem (Prelude.map (\(curKey, curVal) ->
                                                  if curKey == key then 
                                                   (key, value) 
                                                  else (curKey, curVal)) arr)
         ))

-- | Just return size of HM
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable vect size) = mask_ (readTVarIO size)