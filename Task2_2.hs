module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f (f ini x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini [] = ini
foldr f ini (x:xs) = x `f` foldr f ini xs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
  helper (Just (x, ini')) = x : unfoldr f ini'
  helper Nothing = []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x -> (:) (f x)) [] 
 
-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1 

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f [] lst
  where 
    f Nothing r = r
    f (Just x) r = x:r

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd $ foldr (\x (index, d) -> (index-1, (getByIndex index x) : d)) (length lst-1, []) lst
  where 
    getByIndex 0 (h:t) = h
    getByIndex index (h:t) = getByIndex (index-1) t

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = foldr (\x r -> if not (p x) then x:r else r) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el = foldr (\x r-> x == el || r) False 

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if x >= to then Nothing else Just(x, x+step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldr (:) lst2 lst1

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = let(r,t,_) = foldr (\x (result, tmp, index) -> if index == n then (tmp:result, [x], 1)
                                                              else (result, x:tmp, index+1)) ([],[],0) lst
              in t:r

