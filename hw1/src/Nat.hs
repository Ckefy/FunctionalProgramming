{-# LANGUAGE InstanceSigs #-}
--Task 1.2
module Nat where

--1 Сложение двух натуральных чисел.  DONE
--2 Умножение двух натуральных чисел. DONE
--3 Вычитание натуральных чисел.      DONE
--4 Превращение целых чисел в натуральные и наоборот. DONE
--5 Проверка натуральных чисел на равенство. DONE
--6 Сравнение натуральных чисел. DONE

--7 Проверка натурального числа на чётность. DONE
--8 Целочисленное деление натуральных чисел. DONE
--9 Остаток от деления натурального числа на другое. DONE

data Nat = Z
         | S Nat
         deriving (Show)

instance Num Nat where
    --1
    -- | Addition of two Nats
    (+) :: Nat -> Nat -> Nat
    first + Z = first
    Z + second = second
    first + S second = S (first + second)
    --2
    -- | Multiply of two Nats
    (*) :: Nat -> Nat -> Nat
    first * Z = Z
    Z * second = Z
    first * S second = (first * second) + first
    --3
    -- | Subtraction of two Nats
    (-) :: Nat -> Nat -> Nat
    first - Z = first
    Z - second = Z
    (S first) - (S second) = first - second
    --4.1
    -- | Convert Integer number to Nat
    fromInteger :: Integer -> Nat
    fromInteger number
        | number == 0 = Z
        | number < 0  = error "Can`t make Nat out of negative number"
        | otherwise   = S (fromInteger (number - 1))
    --implements for minimal complete definition
    -- | Module of Nat number
    abs :: Nat -> Nat
    abs nat = nat
    --can`t make negate out of Nat
    negate :: Nat -> Nat
    negate = error "Can`t make negate out of Nat"
    --only zero or plus
    -- | Sign function of Nat
    signum :: Nat -> Nat
    signum Z = Z
    signum nat = S Z

--4.2
-- | Convert Nat to Integer number
natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S nat) = natToInteger nat + 1

--5
-- | Eq instance of Nat
instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    (S first) == (S second) = first == second
    Z == Z = True
    (S first) == Z = False
    Z == (S second) = False

--6
-- | Ord instance of Nat
instance Ord Nat where
    compare :: Nat -> Nat -> Ordering
    compare (S first) (S second) = compare first second
    compare Z Z = EQ
    compare Z second = LT
    compare first Z = GT

--ADVANCED
--7 - True if even, False if odd
-- | Check parity of Nat number
parity :: Nat -> Bool
parity (S nat) = not (parity nat)
parity _ = True

--8
-- | Division of two Nats
natDiv :: Nat -> Nat -> Nat
natDiv first second
     | second == Z = error "Can`t divide by zero"
     | first >= second = S (natDiv (first - second) second)
     | otherwise = Z

--9
-- | Remind of two Nats
natMod :: Nat -> Nat -> Nat
natMod first second
     | second == Z = error "Can`t divide by zero"
     | first >= second = natMod (first - second) second
     | otherwise = first