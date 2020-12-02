-- Created on 02 Dec 2020 by richard.stewing@udo.edu

module Day1 where 

import System.IO(readFile)

get_input_1 :: IO [Int]
get_input_1 = do
  input <- readFile "input_1.txt"
  return $ map read (lines input)


calc_solutions_1 :: [Int] -> [Int]
calc_solutions_1 input = do
  x <- input
  y <- input
  if x + y == 2020 then
    return $ x * y
  else
    []

solve_1 :: IO [Int]
solve_1 = do
  input <- get_input_1
  return $ calc_solutions_1 input
  

calc_solutions_2 :: [Int] -> [Int]
calc_solutions_2 input = do
  x <- input
  y <- input
  z <- input
  if x + y + z == 2020 then
    return $ x * y * z
  else
    []


solve_2 :: IO [Int]
solve_2 = do
  input <- get_input_1
  return $ calc_solutions_2 input
