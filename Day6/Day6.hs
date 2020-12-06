-- Created on 06 Dec 2020 by richard.stewing@udo.edu

module Day6 where

import Data.List(nub, intersect)
import System.IO(readFile)

split_records :: String -> [String]
split_records txt = l_r:records where (l_r, records) = cut_on_blank_line txt

cut_on_blank_line :: String -> (String, [String])
cut_on_blank_line "" = ("", [])
cut_on_blank_line ('\n':'\n':txt) = ("", l_r:records) where (l_r, records) = cut_on_blank_line txt
cut_on_blank_line ('\n':txt) = (' ':l_r, records) where (l_r, records) = cut_on_blank_line txt
cut_on_blank_line (c:txt) = (c:l_r, records) where (l_r, records) = cut_on_blank_line txt

get_input :: IO [String]
get_input = readFile "input.txt" >>= return . split_records

solve_1 :: IO Int
solve_1 = get_input >>= return . sum . map (length . nub . filter (/=' ')) 

solve_2 :: IO Int
solve_2 = get_input >>= return . sum . map (length . foldl1 intersect . words) 
