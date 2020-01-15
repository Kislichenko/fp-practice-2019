module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operation = Plus | Minus | Multiply deriving(Show, Eq)
data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op::Operation, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant $ l + r
(|+|) l r = BinaryTerm l Plus r
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant $ l - r
(|-|) l r = BinaryTerm l Minus r
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant $ l * r
(|*|) l r = BinaryTerm l Multiply r

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = replacer expression varName replacement

replacer :: Term -> String -> Term -> Term
replacer (IntConstant constant) varName replacement = (IntConstant constant)
replacer (Variable var) varName replacement = if var == varName then replacement else (Variable var)
replacer (BinaryTerm l op r) varName replacement = BinaryTerm left op right where
    left = replacer l varName replacement
    right = replacer r varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = counter expression
counter (IntConstant constant) = IntConstant constant
counter (BinaryTerm l op r) = error ("Expression expression contains variables!")

