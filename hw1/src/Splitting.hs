module Splitting where
--Task 2.2
import Data.List.NonEmpty
import Data.Foldable
import Data.Maybe (fromJust)
import Data.List.NonEmpty as Lst (uncons)

--base modification, returns NonEmpty!
--if current element is separator, then just move on our cell in row, else add it to current cell
--start elem - [] :| []
--foldr f start lst = lst[-2] f (lst[-1] f start)
-- | Split list into sublists (by separator) with fold
splitOn :: Eq store => store -> [store] -> NonEmpty [store]
splitOn separator = foldr (\cur (x :| xs) -> if cur == separator
                                                then [] :| (x : xs)
                                                    else (cur : x) :| xs) ([] :| [])

--advanced modification
--(start f lst[0]) f lst[1]
--foldl f start lst
-- | Join sublists into list sepating with special symbol with fold
joinWith :: store -> NonEmpty [store] -> [store]
joinWith separator lst = foldl (\joined cur -> joined ++ (separator : cur)) first (fromJust others)
    where
        (first, others) = uncons lst