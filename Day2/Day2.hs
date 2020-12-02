-- Created on 02 Dec 2020 by richard.stewing@udo.edu

module Day2 where

import Data.List(elemIndex)
import System.IO(readFile)

type Input = ((Int, Int), Char, String)

idx x xs = i
  where
    Just i = elemIndex x xs

countElem x = foldr (\y c -> if x == y then c + 1 else c) 0 

parse_line :: String -> Input
parse_line l = ((min, max), char, password)
  where
    split_min_max = idx '-' l
    min = read $ take split_min_max l
    split_white_space = idx ' ' l
    max = read $ drop (split_min_max + 1) $ take split_white_space l
    password = drop (idx ':' l+2) l
    char = head $ drop (split_white_space + 1) l

get_input_1 :: IO [Input]
get_input_1 = do
  input <- readFile "input_1.txt"
  return $ map parse_line (lines input)

validate_input_1 :: Input -> Bool
validate_input_1 ((min, max), c, p) = elem (countElem c p) [min..max]

bool2bin :: Bool -> Int
bool2bin b = if b then 1 else 0

solve :: (Input -> Bool) -> IO Int
solve f = do
  input <- get_input_1
  return $ sum $ map (bool2bin . f) input


solve_1 :: IO Int
solve_1 = solve validate_input_1

eq_at 0 x (y:ys) = x == y
eq_at 0 _ _      = False
eq_at i x (y:ys)
  | i < 0        = False
  | otherwise    = eq_at (i-1) x ys
eq_at _ _   _    = False

xor :: Bool -> Bool -> Bool
xor b1 b2 = if b1 then not b2 else b2

validate_input_2 :: Input -> Bool
validate_input_2 ((min, max), c, p) = eq_at (min - 1) c p `xor` eq_at (max - 1) c p

solve_2 :: IO Int
solve_2 = solve validate_input_2
