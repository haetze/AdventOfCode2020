-- Created on 10 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3

module Day10 where 

import System.IO(readFile)
import Data.List(sort)
import Data.Map
import Data.Maybe(fromJust)

get_input :: IO [Int]
get_input = readFile "input.txt" >>= return . (\l -> l ++ [maximum l + 3]) . sort . Prelude.map read . lines

sol_1 :: Int -> [Int] -> (Int, Int)
sol_1 n [] = (0,0)
sol_1 n (x:xs)
  | x == n + 1 = (c1 + 1, c3) 
  | x == n + 3 = (c1, c3 + 1)
  | otherwise  = (c1, c3)
  where (c1, c3) = sol_1 x xs

solve_1 = get_input >>= return . uncurry (*) . sol_1 0 

type Input = (Int, [Int])
type M = Map Input Integer

sol_2 :: Int -> M -> [Int] -> (M, Integer)
sol_2 n m []
  | member (n, []) m = (m, fromJust $ Data.Map.lookup (n, []) m)
  | otherwise        = (insert (n, []) 1 m, 1)
sol_2 n m [x] 
  | member (n, [x]) m = (m, fromJust $ Data.Map.lookup (n, [x]) m)
  | x > n + 3         = (insert (n, [x]) 0 m, 0)
  | otherwise         = (insert (n, [x]) 1 m, 1)
sol_2 n m (x:y:xs)
  | member (n, x:y:xs) m = (m, fromJust $ Data.Map.lookup (n, x:y:xs) m)
  | x > n + 3            = (insert (n, x:y:xs) 0 m, 0)
  | y > n + 3            = (m_1, r_1)
  | otherwise            = (insert (n, x:y:xs) s m_3, s)
  where
    s          = r_1 + r_2 + r_3    
    (m_1, r_1) = sol_2 x m (y:xs)
    (m_2, r_2) = sol_2 y m_1 xs
    (m_3, r_3) = sol_2 n m_2 xs

solve_2 = get_input >>= return . snd . sol_2 0 empty


