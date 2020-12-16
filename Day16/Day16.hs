-- Created on 16 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,16 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day16 where

import System.IO(readFile)
import Data.List(intersect)

constraintsF    = "input_contraints.txt"
your_ticketF   = "input_your_ticket.txt"
other_ticketsF = "input_other_tickets.txt"

parse_constraint txt = (class_name, elements)
  where
    class_name = takeWhile (/=':') txt
    range_S = tail $ tail $ dropWhile (/=':') txt
    elements = parse_range range_S

parse_range :: String -> [Integer]
parse_range [] = []
parse_range txt = range_1 ++ parse_range txt'
  where
    txt_range_1 = takeWhile (/=' ') txt
    lower_limit_1 = read $ takeWhile (/='-') txt
    upper_limit_1 = read $ takeWhile (/=' ') $ tail $ dropWhile (/='-') txt
    range_1 = [lower_limit_1..upper_limit_1]
    txt' = drop 2 $ dropWhile (/='r') txt

get_constraints = readFile constraintsF >>= return . map parse_constraint . lines

get_your_ticket :: IO [Integer]
get_your_ticket = readFile your_ticketF >>= return . read . (++"]") . ("["++)

get_other_tickets :: IO [[Integer]]
get_other_tickets = readFile other_ticketsF >>= return . map (read . (++"]") . ("["++)) . lines

filter_ticket_values :: [(String, [Integer])] -> [Integer] -> [Integer]
filter_ticket_values c values = filter (\v -> not (any (\(_, e) -> v `elem` e) c)) values 

solve_1 = do
  c <- get_constraints
  o <- get_other_tickets
  return $ sum $ map sum $ map (filter_ticket_values c) o

filter_tickets :: [(String, [Integer])] -> [[Integer]] -> [[Integer]]
filter_tickets c ts = filter (\vs -> filter_ticket_values c vs == []) ts

get_useful_tickets = do
  c <- get_constraints
  o <- get_other_tickets
  return $ filter_tickets c o

match :: [(String, [Integer])] -> Integer -> [String]
match c v = foldr (\(n, r) ls -> if v `elem` r then n:ls else ls) [] c

match_ticket :: [(String, [Integer])] -> [Integer] -> [[String]]
match_ticket c vs = map (match c) vs

combine_tickets_match :: [[String]] -> [[String]] -> [[String]]
combine_tickets_match [] [] = []
combine_tickets_match (m:ms) (m':ms') = intersect m m' : combine_tickets_match ms ms'
combine_tickets_match _ _ = error "different length ticket match"

order_fields = do
  t <- get_useful_tickets
  c <- get_constraints
  return $ map head $ fix $ foldr1 combine_tickets_match $ map (match_ticket c) t

singles :: [[String]] -> [String]
singles = map head . filter (\l -> length l == 1)

remove_when_not_alone _ [] = []
remove_when_not_alone n (c:cs) = if length c == 1 then c:cs' else filter (/= n) c : cs'
  where cs' = remove_when_not_alone n cs

remove_all_when_not_alone [] cs = cs
remove_all_when_not_alone (n:ns) cs = remove_all_when_not_alone ns (remove_when_not_alone n cs)

fix cs = if cs' == cs then cs else fix cs'
  where
    cs' = remove_all_when_not_alone (singles cs) cs

isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = x == y && xs `isPrefixOf` ys

solve_2 = do
  order <- order_fields
  your_ticket <- get_your_ticket
  return $ product $ map snd $ filter (\(n,_) -> "departure" `isPrefixOf` n) $ zip order your_ticket
