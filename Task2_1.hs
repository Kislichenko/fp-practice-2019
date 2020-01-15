module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}
import Prelude hiding (lookup)
import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
                | Node Integer v (TreeMap v) (TreeMap v) deriving (Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = case t of 
  EmptyTree -> False
  (Node key v l r) 
    | k == key -> True
    | k < key -> contains l key
    | k > key -> contains r key

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = case t of
  EmptyTree -> error ("There is no value for your key!")
  (Node key v l r) 
    | k == key -> v
    | k < key -> lookup k l
    | k > key -> lookup k r

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = case t of
  EmptyTree -> Node k v EmptyTree EmptyTree
  (Node key value l r)
    | k == key -> Node key v l r
    | k < key -> Node key value (insert (k, v) l) r
    | k > key -> Node key value l (insert (k, v) r)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = case t of
  (Node key value l r)
    | i < key -> Node key value (remove i l) r
    | i > key -> Node key value l (remove i r)
    | i == key -> if isEmpty r then l
        else Node keyMinRight valueMinRight l (remove keyMinRight r)
          where 
            isEmpty EmptyTree = True
            isEmpty _ = False
            (keyMinRight, valueMinRight) = findMinKey r
            findMinKey (Node key value l r) = if isEmpty l then (key, value) 
              else findMinKey l
    
-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = case t of
  EmptyTree -> error ("Key was not found!")
  (Node key value l r) 
    | key == i -> (key, value)
    | key > i -> nearestLE i l
    | key < i -> case r of
        EmptyTree -> (key, value)
        (Node k v _ _) 
          | k == i -> (k, v)
          | otherwise -> nearestLE i r
      
-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case t of
  EmptyTree -> []
  (Node key value l r) -> listFromTree l ++ listFromTree r ++ [(key, value)]

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = case t of
  EmptyTree -> error ("Incorrect k")
  (Node key value l r)
    | sizeLeft == i -> (key, value)
    | sizeLeft > i -> kMean i l
    | sizeLeft < i -> kMean (i - sizeLeft - 1) r
      where 
        sizeLeft = sizeOfTree l
        sizeOfTree EmptyTree = 0
        sizeOfTree (Node _ _ left right) = 1 + sizeOfTree left + sizeOfTree right
