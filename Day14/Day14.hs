-- Created on 14 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,14 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day14 where

import System.IO(readFile)
import qualified Data.Map as M

inputF = "input.txt"

type BitInt = [Int]

to_bits :: Int -> Int -> BitInt
to_bits n p
  | p < 0 && n == 0 = []
  | p < 0           = error "Number not eq 0 at the end"
  | n >= 2^p        = 1 : to_bits (n-2^p) (p-1)
  | otherwise       = 0 : to_bits n (p-1) 

to_int :: BitInt -> Int
to_int [] = 0
to_int bits = (last bits) + (2*to_int  (init bits))


data BitMaskField = X | B Int deriving(Show, Read, Eq)
type BitMask = [BitMaskField]

empty :: Int -> BitMask
empty n = take n $ repeat X

combine :: BitInt -> BitMask -> BitInt
combine [] []           = []
combine (n:t) (X:t')    = n : combine t t'
combine (n:t) (B n':t') = n' : combine t t'
combine b m             = error $ "Error in Combine bits: " ++ show b ++ ", mask: " ++ show m

data Op = Mask BitMask | Assign Int BitInt deriving(Show, Read, Eq)

c_to_mask :: Char -> BitMaskField
c_to_mask 'X' = X
c_to_mask '0' = B 0
c_to_mask '1' = B 1 
c_to_mask  x  = error $ "Unexpected char: " ++ [x]

parse_line :: String -> Op
parse_line ('m':'a':'s':'k':' ':'=':' ':txt) = Mask $ map c_to_mask txt
parse_line ('m':'e':'m':'[':txt) = Assign addr bits
  where
    addr_s = takeWhile (/=']') txt
    addr = read addr_s
    bits = to_bits (read $ tail $ dropWhile (/='=') txt) 35

get_input = readFile inputF >>= return . map parse_line . lines

eval :: M.Map Int Int -> BitMask -> [Op] -> (M.Map Int Int, BitMask)
eval mem mask [] = (mem, mask)
eval mem mask (Mask mask':ops) = eval mem mask' ops
eval mem mask (Assign addr bits:ops) = eval (M.insert addr (to_int $ combine bits mask) mem) mask ops

solve_1 = get_input >>= return . sum . fst . eval M.empty (empty 35)

empty' n = take n $ repeat (B 0)

combine' :: BitInt -> BitMask -> [BitInt]
combine' [] [] = [[]]
combine' (n:t) (X:t') = do
  addr_t <- combine' t t'
  [0 : addr_t, 1 : addr_t]
combine' (n:t) (B 1:t') = do
  addr_t <- combine' t t'
  [1 : addr_t]
combine' (n:t) (B 0:t') = do
  addr_t <- combine' t t'
  [n : addr_t]
combine' b m = error $ "Error in Combine bits: " ++ show b ++ ", mask: " ++ show m

eval' :: M.Map Int Int -> BitMask -> [Op] -> (M.Map Int Int, BitMask)
eval' mem mask [] = (mem, mask)
eval' mem mask (Mask mask':ops) = eval' mem mask' ops
eval' mem mask (Assign addr bits:ops) = eval' mem' mask ops
  where
    n = to_int bits
    addr_es = combine' (to_bits addr 35) mask
    mem' = foldr (\a m -> M.insert (to_int a) n m) mem addr_es

solve_2 = get_input >>= return . sum . fst . eval' M.empty (empty' 35)
