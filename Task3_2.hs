module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList a = case a of
  RNil -> []
  RCons x y -> (rlistToList x) ++ [y]

listToRList :: [a] -> ReverseList a
listToRList lst = case lst of
  [] -> RNil
  (head:tail) -> RCons (listToRList tail) head

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
  show RNil = "RNil"
  show (RCons x y) = "RCons (" ++ show x ++ ") " ++ show y 

instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) RNil (RCons x y) = False
  (==) (RCons x y) RNil = False
  (==) (RCons x1 y1) (RCons x2 y2) = (x1 == x2) && (y1 == y2)

instance (Ord a) => Ord(ReverseList a) where
  compare RNil RNil = EQ
  compare RNil (RCons x y) = LT
  compare (RCons x y) RNil = GT
  compare (RCons x1 y1) (RCons x2 y2) 
    | x1 == x2 && y1 == y2 = EQ
    | x1 > x2 = GT
    | x1 < x2 = LT
    | x1 == x2 && y1 > y2 = GT
    | x1 == x2 && y1 < y2 = LT

instance Semigroup (ReverseList a) where
  (<>) x RNil = x
  (<>) RNil y = y
  (<>) x (RCons x1 y1) = RCons (x <> x1) y1
  
instance Monoid (ReverseList a) where
  mempty = RNil

instance Functor ReverseList where
  fmap f (RNil) = RNil
  fmap f (RCons x y) = RCons (fmap f x) (f y)
