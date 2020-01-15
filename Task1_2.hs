module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}
import Prelude hiding (cos, sin, gcd)

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = fromInteger (round $ (sincounter 10 0 x)* 1e6) / 1e6

sincounter :: Double -> Double -> Double -> Double
sincounter 0 accum x = accum + x
sincounter n accum x = sincounter (n-1) ((-1)**n * (x**(2*n+1)/ factorial(2*n+1))+accum) x

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = fromInteger (round $ (coscounter 10 0 x)* 1e6) / 1e6

coscounter :: Double -> Double -> Double -> Double
coscounter 0 accum x = accum + 1
coscounter n accum x = coscounter (n-1) ((-1)**n * (x**(2*n)/ factorial(2*n))+accum) x

factorial :: Double -> Double
factorial n = foldl (*) 1 [1..n]

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd x y = gcd y (x `mod` y)


-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = foldl check True [2..x `div` 2] where
    check acc d = if x `mod` d == 0 then False else acc 
  

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
