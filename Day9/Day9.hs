-- Created on 09 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3

module Day9 where 

import System.IO(readFile)

type Data = [([Int], Int)]

take25 :: [Int] -> [[Int]]
take25 [] = []
take25 xs = take 25 xs : (take25 $ tail xs)

get_input = readFile "input.txt" >>= return . (\i -> zip (take25 i) (drop 25 i)) . map read . lines

filter_non_valid :: ([Int],Int) -> (Bool,Int)
filter_non_valid (xs,x) = (x `elem` all_sums, x)
  where all_sums = xs >>= (\a -> xs >>= (\b -> return $ a+b))

solve_1 = get_input >>= return . invalid_n

invalid_n = head . dropWhile (\(b,_) -> b) . map filter_non_valid

all_ranges' :: [Int] -> [[Int]]
all_ranges' xs = [2..length xs] >>= \i -> return $ take i xs

all_ranges []     = []
all_ranges (x:xs) = all_ranges' (x:xs) ++ all_ranges xs

correct_sum x = head . dropWhile (\l -> sum l /= x) . all_ranges

min_max l = (minimum l, maximum l)

solve_2 = do
  input <- readFile "input.txt"
  let numbers = map read $ lines input
  (b,n) <- solve_1
  let s_range = correct_sum n numbers
  let (min, max) = min_max s_range
  return $ min + max
