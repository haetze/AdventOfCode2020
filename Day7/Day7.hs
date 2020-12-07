-- Created on 07 Dec 2020 by Richard Stewing<richard.stewing@udo.edu>
-- Copyright Richard Stewing, 2020
-- Licensed under GPLv3

module Day7 where 
import System.IO(readFile)
import Data.List(nub)

type BagGraph = [(String, (Int, String))]

indexes :: Eq a => a -> [a] -> Int -> [Int]
indexes x [] _ = []
indexes x (y:ys) i
  | x == y    = i : indexes x ys (i+1)
  | otherwise =     indexes x ys (i+1)

read_m "contain" = 0
read_m t         = read t

parse_line :: String -> BagGraph
parse_line txt =
  [(start_color, (read_m (dropped_start !! (i - 3)),
                  dropped_start !! (i - 2) ++
                  " " ++
                  dropped_start !! (i - 1)))
  | i <- bag_p ++ bags_p] 
  where
    splitted_txt = map (filter (\c -> c /= '.' && c /= ',')) $ words txt
    dropped_start = drop 3 splitted_txt
    start_color = unwords $ take 2 splitted_txt
    bag_p = indexes "bag" dropped_start 0
    bags_p = indexes "bags" dropped_start 0

get_input :: IO BagGraph
get_input = readFile "input.txt" >>= return . concat . map parse_line . lines 

colors :: BagGraph -> [String]
colors graph = nub $ [c | (c, _) <- graph] ++ [c | (_,(_,c)) <- graph]

find_paths :: [String] -> BagGraph -> [[String]]
find_paths xs@("shiny gold":_) g = [xs]
find_paths xs@(c:_) g = do
  (v, (n, v')) <- g
  if v == c && n > 0 && not (v' `elem` xs) then find_paths (v':xs) g else [] 

start_colors :: [[String]] -> [String]
start_colors = nub . map last

all_start_colors :: BagGraph -> [String]
all_start_colors g = start_colors $ concatMap (\c -> find_paths [c] g) $ colors g 

-- minus 1 because you can start with shiny gold without it containing shiny gold
-- returns 142 but runs very long
solve_1 = get_input >>= return . (+(-1)) . length . all_start_colors 

n_bags :: [(Int, String)] -> BagGraph -> [[(Int, String)]]
n_bags xs@((m,c):_) g = do
  (v, (n, v')) <- g
  if v == c && n > 0 && not (v' `elem` map snd xs) then n_bags ((m*n,v'):xs) g else [xs] 

drop_along :: [(Int, String)] -> [String] -> [(Int, String)]
drop_along [] _ = []
drop_along p@((_, e):ps) [] = p
drop_along p@((_, e):ps) (e':es)
  | e == e'   = drop_along ps es
  | otherwise = p

find_smallest_remaining :: [(Int, String)] -> [[String]] -> [(Int, String)]
find_smallest_remaining p ps = foldl f p (map (drop_along p) ps)
  where
    f p p'
      | length p > length p' = p'
      | otherwise            = p

fold_path :: [(Int, String)] -> [[String]] -> Int
fold_path p ps = sum $ map fst $ find_smallest_remaining p ps 

fold_paths :: [[(Int, String)]] -> [[String]] -> Int
fold_paths [] _ = 0
fold_paths (p:ps) paths = fold_path p paths + fold_paths ps (nub (map snd p : paths)) 

-- minus 1 because the gold bag doesn't count
solve_2 = get_input >>= return . (+(-1)) . (flip fold_paths []) . map reverse . n_bags [(1, "shiny gold")]
