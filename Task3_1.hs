module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where 
  show Zero = show "0"
  show (Succ x) = show ("+1") ++ show x
  show (Pred x) = show ("-1") ++ show x

instance Eq WeirdPeanoNumber where
  (==) Zero Zero = True
  (==) Zero (Succ x) = False
  (==) (Succ x) Zero = False
  (==) Zero (Pred x) = False
  (==) (Pred x) Zero = False
  (==) (Succ x) (Succ y) = if x==y then True else False
  (==) (Pred x) (Pred y) = if x==y then True else False
  (==) (Succ x) (Pred y) = False
  (==) (Pred x) (Succ y) = False

instance Ord WeirdPeanoNumber where 
  compare Zero Zero = EQ
  compare Zero (Succ x) = LT
  compare Zero (Pred x) = GT
  compare (Succ x) Zero = GT
  compare (Pred x) Zero = LT
  compare (Pred x) (Succ y) = LT
  compare (Succ x) (Pred y) = GT
  compare (Succ x) (Succ y) = compare x y
  compare (Pred x) (Pred y) = compare x y

instance Num WeirdPeanoNumber where
  negate Zero = Zero
  negate (Succ x) = Pred (negate x)
  negate (Pred x) = Succ (negate x)

  (+) Zero y = y
  (+) x (Succ y) = Succ (x + y)
  (+) x (Pred y) = Pred (x + y)
  (+) x y = y + x

  (*) Zero _ = Zero
  (*) x (Succ y) = x * y + x
  (*) x (Pred y) = x * y - x
  (*) x y = y * x

  signum Zero = Zero
  signum (Succ _) = Succ Zero
  signum (Pred _) = Pred Zero

  fromInteger 0 = Zero 
  fromInteger x = if x > 0 then Succ (fromInteger x-1) else Pred (fromInteger x+1)

  abs Zero = Zero
  abs (Succ x) = Succ x
  abs (Pred x) = Succ (abs x)

instance Enum WeirdPeanoNumber where
  toEnum x = fromInteger (toInteger x)
  fromEnum x = fromIntegral (toInteger x)

instance Real WeirdPeanoNumber where 
  toRational x = toRational (toInteger x)

instance Integral WeirdPeanoNumber where
  quotRem x y = let (f, s) = if (toInteger y) == 0 then error ("Division by Zero") 
                             else quotRem (toInteger x) (toInteger y)
                in (integerToPeanoNumber f, integerToPeanoNumber s)
  toInteger x = peanoToInteger x 0

peanoToInteger :: WeirdPeanoNumber -> Integer -> Integer
peanoToInteger (Succ x) accum = peanoToInteger x (accum+1)
peanoToInteger (Pred x) accum = peanoToInteger x (accum-1)
peanoToInteger Zero accum = accum 

integerToPeanoNumber :: Integer -> WeirdPeanoNumber
integerToPeanoNumber num 
  | num == 0 = Zero
  | num > 0 = Succ $ integerToPeanoNumber (num-1)
  | num < 0 = Pred $ integerToPeanoNumber (num+1)