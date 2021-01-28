module Concats where
--Task 3.1
import Data.Maybe (fromMaybe)

--lambda convert from Maybe m to Monoid (list)
-- | Concat all lists in maybes into one list 
maybeConcat :: Monoid m => [Maybe m] -> m
maybeConcat = foldMap (fromMaybe mempty)

-- | Concat all eithers into one tuple
eitherConcat :: Monoid m1 => Monoid m2 => [Either m1 m2] -> (m1, m2)
eitherConcat = foldMap (\elem -> case elem of
                                    Left left -> (left, mempty)
                                    Right right -> (mempty, right))