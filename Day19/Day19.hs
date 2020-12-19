-- Created on 19 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,19 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day19 where

import System.IO(readFile)
import qualified Data.Map as M


type RID = Int
data Rule = Rule :+ Rule | Rule :. Rule | R RID | C Char deriving(Show, Read, Eq) 

inputF = "input.txt"
inputF2 = "input2.txt"

parse_rule :: String -> Rule
parse_rule txt = parse_from_words $ words txt

parse_from_words ws = if rest == [] then seq else seq :+ parse_from_words (tail rest)
  where
    seq       = parse_seq first_seq
    first_seq = takeWhile (/= "|") ws
    rest      = dropWhile (/= "|") ws

parse_seq (('\"':c:"\""):rest)
  | rest == [] = C c
  | otherwise  = C c :. parse_seq rest
parse_seq (n:rest)
  | rest == [] = R $ read n
  | otherwise  = R (read n) :. parse_seq rest

parse_line :: String -> M.Map RID Rule -> M.Map RID Rule
parse_line line m = M.insert rid rule m
  where
    rid = read $ takeWhile (/=':') line
    rule = parse_rule $ tail $ dropWhile (/=':') line

get_map f = readFile f >>= return . foldr (\l m -> parse_line l m) M.empty . takeWhile (/="") . lines

get_words f = readFile f >>= return . tail . dropWhile (/="") . lines

eval_rule :: Rule -> M.Map RID Rule -> String -> [String]
eval_rule (C c) m "" = []
eval_rule (C c) m (c':w)
  | c == c'   = [w]
  | otherwise = []
eval_rule (R 999999) m "" = [""]
eval_rule (R 999998) m "" = []
eval_rule (R rid) m w = case M.lookup rid m of
                          Nothing -> error "Unkown rule referenced"
                          Just r  -> eval_rule r m w
eval_rule (r :. r') m w = eval_rule r m w >>= eval_rule r' m
eval_rule (r :+ r') m w = eval_rule r m w ++ eval_rule r' m w

valid_by_rule r m w = any (\w' -> w' == "") (eval_rule r m w)

solve_1 = do
  rules <- get_map inputF
  ws    <- get_words inputF
  return $ sum $ map (fromEnum . valid_by_rule (R 0) rules) ws

solve_2 = do
  rules <- get_map inputF2
  ws    <- get_words inputF2
  return $ sum $ map (fromEnum . valid_by_rule (R 0) rules) ws

