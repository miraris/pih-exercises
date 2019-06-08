{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (x:xs) (y:ys)
  | x == y = 1 + ms
  | otherwise = ms
  where
    ms = exactMatches xs ys
exactMatches _ _ = 0

exactMatches' :: Code -> Code -> Int
exactMatches' xs ys = length . filter cmp $ zip xs ys
  where
    cmp = uncurry (==)

--exactMatches a b =
--  sum $
--  zipWith
--    (\x y ->
--       if x == y
--         then 1
--         else 0)
--    a
--    b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map cnt colors
  where
    cnt x = (length . filter (== x)) xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = length $ intersect xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code

-- getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] ==Move [Red, Orange, Orange, Blue] 1 2
-- newMove exactMatches  matches - exactMatches

getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
