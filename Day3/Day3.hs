-- Created on 03 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3
module Day3 where

import System.IO(readFile)

data Field = Tree | Free | Bot
  deriving(Show, Eq)


char_to_field :: Char -> Field
char_to_field '_' = Bot
char_to_field '.' = Free
char_to_field '#' = Tree
char_to_field _   = error "Unkonw symbol in Input"


parse_line :: String -> [Field]
parse_line [] = []
parse_line xs = map char_to_field xs ++ parse_line xs

get_input_1 :: IO [[Field]]
get_input_1 = do
  input <- readFile "input_1.txt"
  return $ map parse_line (lines input) ++ repeat (repeat Bot)

--             Board
--                           Location
--                                       Step Down
--                                         |     Step Right
--                                         |      |     Current Count
--                                         |      |      |
count_trees :: [[Field]] -> (Int, Int) -> Int -> Int -> Int -> Int
count_trees board (row, column) step_d step_r c
  | board !! row !! column == Bot  = c
  | board !! row !! column == Tree = count_trees board (row+step_d, column + step_r) step_d step_r (c+1) 
  | otherwise                      = count_trees board (row+step_d, column + step_r) step_d step_r c


solve_1 :: IO Int
solve_1 = do
  board <- get_input_1
  return $ count_trees board (0, 0) 1 3 0

slopes :: [(Int, Int)]
slopes = [(1,1) ,(1,3) ,(1,5) ,(1,7) ,(2,1)]

solve_2 :: IO Int
solve_2 = do
  board <- get_input_1
  return $ product $ map (map_f board) slopes
    where
      map_f board (step_d, step_r) = count_trees board (0,0) step_d step_r 0


