module SMA where

import Data.List
import Control.Monad.State

--real is int, float, double
--fractional is for only float and double
--in list we can have all types, while in the answer only fractional
-- | Find simple simple moving average via classical algorithm
-- | But only using State monad
moving :: (Real a1, Fractional a2) => Int -> [a1] -> [a2]
moving number lst = evalState (getState lst) []
  where
    getState :: (Real a1, Fractional a2) => [a1] -> State [a1][a2]
    getState (x : xs) = do
      xsRes <- get
      let curWindow = take number (x : xsRes)
      let average = case curWindow of
                        [] -> 0
                        _ -> realToFrac (sum curWindow) / genericLength curWindow
      return (average : evalState (getState xs) (take number (x : xsRes)))
    getState [] = return []