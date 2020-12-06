-- Created on 05 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3
module Day5 where


import System.IO(readFile)

parse_line :: String -> Int
parse_line txt = foldl (\a b -> a*2 + b) 0 (map f txt)
  where
    f 'F' = 0
    f 'B' = 1
    f 'R' = 1
    f 'L' = 0
    f  c  = error $ "unkown letter " ++ [c]

get_input :: IO [Int]
get_input = readFile "input.txt" >>= return . map parse_line . lines 

solve_1 :: IO Int
solve_1 = get_input >>= return . maximum 

find_missing :: [Int] -> [Int]
find_missing ids = do
  id <- [0..2^11-1]
  if not (id `elem` ids) &&
     id + 1 `elem` ids &&
     id - 1 `elem` ids
    then return id else []
    
solve_2 :: IO [Int]
solve_2 = get_input >>= return . find_missing

