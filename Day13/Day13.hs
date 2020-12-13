-- Created on 13 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,13 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day13 where

import System.IO(readFile)
import Control.Monad

inputF = "input.txt"

transform_line []       = []
transform_line ('x':ls) = transform_line ls
transform_line (',':ls) = ' ' : transform_line ls
transform_line (x:ls)   = x : transform_line ls

type ID = Int
type Time = Int

get_input :: IO (Time, [ID])
get_input = do
  (n:ids:[]) <- fmap lines $ readFile inputF
  return $ (read n, map read $ words $ transform_line ids)

departures_after time ids = zip ids $ map (head . filter (>=time)) [[0,id..time+id] | id <- ids] 

wait_times time (id, dep_time) = (id, dep_time - time)

minimum_of_second [x] = x
minimum_of_second ((id, t):xs) = if t < t' then (id, t) else (id', t')
  where (id', t') = minimum_of_second xs

solve_1 = do
  (t, ids) <- get_input
  return $ uncurry (*) $ minimum_of_second $ map (wait_times t) $departures_after t ids

transform_line_2 [] l        = [l]
transform_line_2 (',':txt) l = l : transform_line_2 txt []
transform_line_2 (x:txt) l   = transform_line_2 txt (l ++ [x])

data Input = X | Id Integer deriving(Show, Read, Eq)

parse_line "x" = X
parse_line txt = Id $ read txt

get_input_2 = do
  (_:ids:[]) <- fmap lines $ readFile inputF
  return $ map parse_line $ transform_line_2 ids ""


smallest = 100000000000000

find :: Integer -> Integer -> Integer -> Integer -> [Input] -> Integer
find start _ step_size _ [] = start
find start current step_size done (X:ids) = find start (current+1) (step_size) (done+1) ids
find start current step_size done (Id n:ids)
  | current `mod` n == 0 = find start (current+1) (step_size * n) (done+1) ids
  | otherwise            = find (current-done+step_size) (current+step_size) step_size done (Id n:ids) 

solve_2 = get_input_2 >>= return . find smallest smallest 1 0
