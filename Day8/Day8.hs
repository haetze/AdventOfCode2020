-- Created on 08 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3

module Day8 where 
import System.IO(readFile)
import Data.List(nub)

data Op = NoOp Int | Acc Int | Jmp Int deriving(Show, Read, Eq)

read_m ('+':n) = read n
read_m n = read n 

parse_line ('a':'c':'c':' ':n) = Acc (read_m n)
parse_line ('j':'m':'p':' ':n) = Jmp (read_m n)
parse_line ('n':'o':'p':' ':n) = NoOp (read_m n)

get_input = readFile "input.txt" >>= return . map parse_line . lines

eval :: [Int] -> Int -> [Op] -> Either String String
eval (pc:pcs) acc program
  | pc `elem` pcs          = Right $ "Call to same instruction twice. Acc: " ++ show acc
  | pc < length program    = case program !! pc of
                               NoOp n -> eval (pc+1:pc:pcs)  acc      program
                               Acc  n -> eval (pc+1:pc:pcs) (acc + n) program
                               Jmp  n -> eval (pc+n:pc:pcs)  acc      program
  | pc == length program   = Right $ "Program terminated. Acc: " ++ show acc
  | otherwise              = Left $  "program counter (" ++ show pc ++ ") out of program"

solve_1 = get_input >>= return . eval [0] 0

switch_op (NoOp n) = Jmp n
switch_op (Jmp n)  = NoOp n
switch_op op       = op

programs p = [0..length p - 1] >>= \i -> return $ take i p ++ [switch_op $ p !! i] ++ drop (i+1) p

filter_sol :: [Either String String] -> [Int]
filter_sol ps = nub [read $ drop (length "Program terminated. Acc: ") i
                    | Right i <- ps
                    , take (length "Program terminated. Acc: ") i == "Program terminated. Acc: "]

solve_2 = get_input >>= return . filter_sol . map (eval [0] 0) . programs
