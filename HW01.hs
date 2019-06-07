{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n > 0 = lastDigit n : (toRevDigits $ dropLastDigit n)
  | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs

-- w/o explicit recursion
mapSecond :: (a -> a) -> [a] -> [a]
mapSecond f = zipWith ($) (cycle [id, f])
-- ($) [id, f, id, f] [2,2,2,2]

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = mapSecond (* 2)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ concatMap toRevDigits ns

-- Exercise 5 -----------------------------------------

luhnDigits :: Integer -> Integer
luhnDigits n =
  sum $
  map
    (\x ->
       if x > 9
         then x - 9
         else x)
    (doubleEveryOther $ toRevDigits n)

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = luhnDigits n `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
