-- Created on 12 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,12 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day12 where

import System.IO(readFile)
import GHC.Float(int2Float
                ,float2Int
                )

inputF = "input.txt"

data Op = N Float | E Float | S Float | W Float | L Int | R Int | F Float
  deriving(Show, Read, Eq)

parse_line ('N':n) = N $ read n
parse_line ('E':n) = E $ read n
parse_line ('S':n) = S $ read n
parse_line ('W':n) = W $ read n
parse_line ('F':n) = F $ read n
parse_line ('L':n) = L $ read n `div` 90
parse_line ('R':n) = R $ read n `div` 90

get_input = readFile inputF >>= return . map parse_line . lines

type Pos = (Float, Float)
data Dir = No | Ea | So | We deriving(Show, Read, Eq, Enum)

eval :: Pos -> Dir -> [Op] -> (Pos, Dir)
eval p d [] = (p, d)
eval p d (L n:ops) = eval p (toEnum ((fromEnum d - n) `mod` 4)) ops
eval p d (R n:ops) = eval p (toEnum ((fromEnum d + n) `mod` 4)) ops
eval (x,y) d@No (F n:ops) = eval (x, y - n)  d ops
eval (x,y) d@So (F n:ops) = eval (x, y + n)  d ops
eval (x,y) d@We (F n:ops) = eval (x - n, y)  d ops
eval (x,y) d@Ea (F n:ops) = eval (x + n, y)  d ops
eval (x,y) d (N n:ops) = eval (x, y - n) d ops
eval (x,y) d (S n:ops) = eval (x, y + n) d ops
eval (x,y) d (W n:ops) = eval (x - n, y) d ops
eval (x,y) d (E n:ops) = eval (x + n, y) d ops

manhatten (x,y) = abs x + abs y 

solve_1 = get_input >>= return . manhatten . fst . eval (0,0) Ea

rot :: Pos -> Float -> Pos
rot (x,y) degree = (int2Float $ round xn, int2Float $ round yn)
  where
    xn = (cos degree) * x - (sin degree) * y
    yn = (sin degree) * x + (cos degree) * y
    

    
eval' :: Pos -> Pos -> [Op] -> Pos
eval' p _ [] = p
eval' p q (L n:ops) = eval' p (rot q (-(pi/2) * int2Float n)) ops
eval' p q (R n:ops) = eval' p (rot q ((pi/2) * int2Float n)) ops
eval' p (a, b) (N n:ops) = eval' p (a, b - n) ops
eval' p (a, b) (S n:ops) = eval' p (a, b + n) ops
eval' p (a, b) (W n:ops) = eval' p (a - n, b) ops
eval' p (a, b) (E n:ops) = eval' p (a + n, b) ops
eval' (x,y) (a, b) (F n:ops) = eval' (x + (n*a), y + (n*b)) (a,b) ops 

solve_2 = get_input >>= return . manhatten . eval' (0,0) (10,-1)
 
  
