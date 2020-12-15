{-# LANGUAGE BangPatterns #-}
-- Created on 15 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,15 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day15 where

import qualified Data.Map as M


length' = foldl (\b _ -> b+1) 0 
input = [1,0,15,2,10,13]
with_idx = zip (init input) [1..]
init_map = M.fromList with_idx
init_e = last input
init_i = length' input + 1 
init_n idx = idx - length' input

next :: Integer -> Integer -> (M.Map Integer Integer) -> Integer
next e i m = case M.lookup e m of
               Just n -> i - n - 1
               Nothing -> 0

n_next :: Integer -> M.Map Integer Integer -> Integer -> Integer -> Integer
n_next e m i 0 = e
n_next e m i n = n_next e' m' (i+1) (n-1)
  where
    !e' = next e i m
    !m' = M.insert e (i-1) m

solve_1 = n_next init_e init_map init_i (init_n 2020)
solve_2 = n_next init_e init_map init_i (init_n 30000000)

