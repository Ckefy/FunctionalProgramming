{-# LANGUAGE InstanceSigs #-}
--Task 1.3
module Plants where

import Data.List.NonEmpty as Lst hiding (sort)
import Data.List (sort, group, length)
import Data.Maybe (fromJust)
import Data.Foldable as F (toList)

--1 Проверка дерева на пустоту. DONE
--2 Подсчёт размера дерева (то есть числа элементов в нём). DONE
--3 Поиск DONE
--4 Вставка DONE
--5 Функцию fromList, которая создаёт дерево из списка элементов. DONE
--6 Функцию, которая удаляет заданный элемент из дерева. DONE

--left right values
data Tree store = List
               | Node (Tree store) (Tree store) (NonEmpty store)
               deriving (Show)

--for tests we impl Eq instance
instance Eq store => Eq (Tree store) where
   (Node left1 right1 values1) == (Node left2 right2 values2) =
     (left1 == left2) && (right1 == right2) && (values1 == values2)
   List == List = True
   _ == _ = False

--1
-- | Check if the tree is empty
empty :: Tree store -> Bool
empty tree = size tree == 0

--2
-- | Size of tree
size :: Tree store -> Int
size List = 0
size (Node left right values) =  size left + size right + Lst.length values

--3
-- | Find element in income tree
find :: Ord store => store -> Tree store -> Maybe store
find cur (Node left right values)
    | Lst.head values == cur = Just (Lst.head values)
    | Lst.head values > cur = find cur left
    | Lst.head values < cur = find cur right
find cur List = Nothing

--4
-- | Add element to income tree
add :: Ord store => store -> Tree store -> Tree store
add cur (Node left right values)
    | Lst.head values == cur = Node left right (Lst.cons cur values)
    | Lst.head values > cur = Node (add cur left) right values
    | Lst.head values < cur = Node left (add cur right) values
add cur List = Node List List (cur :| [])

--5
--grouping by parts
splitValues :: Ord store => [store] -> [[store]]
splitValues lst = Data.List.group (sort lst)

--find the element which must be in Node
findMedium lst = Prelude.splitAt (div (Data.List.length lst) 2) lst
--fromList
-- | Transform list to tree
makeTree :: Ord store => [store] -> Tree store
makeTree lst = parting $ splitValues lst
    where
       parting [] = List
       parting dual = Node (parting left) (parting right) (fromJust (nonEmpty x))
        where
            (left, x:right) = findMedium dual


--6
--if left is list, then answer is this node and right is just connection to this
--if left and right exist, then answer is answer from the left, and connect with new tree
findMostLeft (Node List right values) = (right, values)
findMostLeft (Node left right values) = (Node left1 right values, values1)
    where
        (left1, values1) = findMostLeft left

-- | Delete element from the tree
deleteTree :: Ord store => store -> Tree store -> Tree store
deleteTree cur (Node left right values)
            | Lst.head values == cur = if Data.List.length values > 1 then
                                         Node left right (fromJust others) else
                                            case right of
                                                List -> left
                                                _ -> Node left right1 values1
                                                    where
                                                      (right1, values1) = findMostLeft right
            | Lst.head values > cur = Node (deleteTree cur left) right values
            | Lst.head values < cur = Node left (deleteTree cur right) values
                where
                    (first, others) = uncons values
deleteTree cur List = List

---Task 2.1
instance Foldable Tree where
    -- | it takes the second argument and the last item of the list and applies the function,
    -- | then it takes the penultimate item from the end and the result, and so on
    foldr :: (store -> b -> b) -> b -> Tree store -> b
    foldr f start List = start
    foldr f start (Node left right values) = foldr f (foldr f (foldr f start right) values) left
    -- | Map each element from income structure to monoid, collect them as result
    foldMap :: Monoid m => (store -> m) -> Tree store -> m
    foldMap f List = mempty
    foldMap f (Node left right values) = foldMap f left `mappend` foldMap f values `mappend` foldMap f right