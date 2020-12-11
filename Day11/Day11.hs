-- Created on 11 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 11-12-2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day11 where

import System.IO(readFile)

data Field = Fr | TS | FS deriving(Eq, Show, Read)
type Board = [[Field]]

parse_line txt = Fr : map parse_symbol txt ++ [Fr]

parse_symbol 'L' = FS
parse_symbol '.' = Fr
parse_symbol  _  = error "Error in input"

get_input = readFile "input.txt" >>= f
  where f input_s = return $ above_below : input_d ++ [above_below]
          where
            input_d = map parse_line $ lines input_s
            above_below = take (length $ head input_d) $ repeat Fr 

p_o_f :: Field -> Int
p_o_f f = if f == TS then 1 else 0

person_on_field :: Int -> Int -> Board -> Int
person_on_field line column b = p_o_f $ b !! line !! column 

next :: Int -> Int -> Board -> Field
next line column b
  | b !! line !! column == Fr = Fr
  | b !! line !! column == TS &&
    sum [person_on_field (i+line) (j+column) b | i <- [-1..1], j <- [-1..1]] > 4 = FS
  | b !! line !! column == FS &&
    sum [person_on_field (i+line) (j+column) b | i <- [-1..1], j <- [-1..1]] == 0 = TS
  | otherwise = b !! line !! column

next_board :: (Int -> Int -> Board -> Field) -> Board -> Board
next_board f b = map (\line ->
                        map (\column ->
                               f line column b) [0..length_line-1]) [0..length_columns-1]
  where
    length_line = length $ head b
    length_columns = length b

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

count b = sum $ map sum $ map (\line ->
                                 map (\column ->
                                        person_on_field line column b) line_idx) column_idx
  
  where
    line_idx = [0..length_line-1]
    column_idx = [0..length_columns-1]
    length_line = length $ head b
    length_columns = length b


solve_1 = get_input >>= return . count . fix (next_board next)

next_S :: Int -> Int -> (Int, Int) -> Board -> Field
next_S line column step@(step_l, step_c) b
  | column >= length (head b) ||
    line   >= length       b  ||
    line   <  0               ||
    column <  0                  = Fr
  | b !! line !! column == TS    = TS
  | b !! line !! column == FS    = FS
  | otherwise                    = next_S (line + step_l) (column + step_c) step b


next' :: Int -> Int -> Board -> Field
next' line column b
  | b !! line !! column == Fr = Fr
  | b !! line !! column == TS &&
    sum [p_o_f $ next_S (i+line) (j+column) (i,j) b | i <- [-1..1], j <- [-1..1]] > 5 = FS
  | b !! line !! column == FS &&
    sum [p_o_f $ next_S (i+line) (j+column) (i, j) b | i <- [-1..1], j <- [-1..1]] == 0 = TS
  | otherwise = b !! line !! column

solve_2 = get_input >>= return . count . fix (next_board next')
