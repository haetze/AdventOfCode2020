-- Created on 17 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,17 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day17_2 where

import System.IO(readFile)

inputF = "input.txt"

type Coordinate = (Integer, Integer, Integer,Integer)
type World = Coordinate -> Bool
type FinWorld = [(Coordinate, Bool)]

emptyWorld :: World
emptyWorld _ = False

finWorld2World :: FinWorld -> World
finWorld2World w c = any (\(c', b) -> b && c == c') w

parse_line :: Coordinate -> String -> FinWorld
parse_line    _          []  = []
parse_line (x,y,z,w) ('.':txt) = ((x,y,z,w), False) : parse_line (x+1,y,z,w) txt
parse_line (x,y,z,w) ('#':txt) = ((x,y,z,w), True) : parse_line (x+1,y,z,w) txt
parse_line    _    ( c :txt) = error $ "Unexpected symbol " ++ [c]

parse_lines :: Coordinate -> [String] -> FinWorld
parse_lines    _       []  = []
parse_lines (x,y,z,w) (l:ls) = parse_line (x,y,z,w) l ++ parse_lines (x,y+1,z,w) ls

get_finWorld = readFile inputF >>= return . parse_lines (0,0,0,0) . lines

get_world = get_finWorld >>= return . finWorld2World

get_starting_coordinates = do
  l_l <- fmap (toInteger . length . head . lines) (readFile inputF)
  l_c <- fmap (toInteger . length . lines) (readFile inputF)
  return [(x,y,0,0) | x <- [0..l_l], y <- [0..l_c]]

coordinates_around :: [Coordinate] -> [Coordinate]
coordinates_around cs = [(x,y,z,w) | x <- [min_x - 1 .. max_x + 1]
                                   , y <- [min_y - 1 .. max_y + 1]
                                   , z <- [min_z - 1 .. max_z + 1]
                                   , w <- [min_w - 1 .. max_w + 1]]
  where
    min_x = minimum [x | (x,_,_,_) <- cs]
    max_x = maximum [x | (x,_,_,_) <- cs]
    min_y = minimum [y | (_,y,_,_) <- cs]
    max_y = maximum [y | (_,y,_,_) <- cs]
    min_z = minimum [z | (_,_,z,_) <- cs]
    max_z = maximum [z | (_,_,z,_) <- cs]
    min_w = minimum [w | (_,_,_,w) <- cs]
    max_w = maximum [w | (_,_,_,w) <- cs]


next_state :: World -> Coordinate -> Bool
next_state f c@(x,y,z,w)
  | f c = (sum $ map fromEnum $ [f (x',y',z',w') | x' <- [x-1..x+1]
                                                 , y' <- [y-1..y+1]
                                                 , z' <- [z-1..z+1]
                                                 , w' <- [w-1..w+1]]) `elem` [3,4]
  | otherwise = (sum $ map fromEnum $ [f (x',y',z',w') | x' <- [x-1..x+1]
                                                       , y' <- [y-1..y+1]
                                                       , z' <- [z-1..z+1]
                                                       , w' <- [w-1..w+1]]) == 3

next_world :: World -> [Coordinate] -> World
next_world w cs = finWorld2World $ [(c, next_state w c) | c <- cs]

cycle_n_times :: Integer -> World -> [Coordinate] -> (World, [Coordinate])
cycle_n_times 0 w cs = (w, cs)
cycle_n_times n w cs = cycle_n_times (n-1) w' cs'
  where
    w' = next_world w cs'
    cs' = coordinates_around cs

count_active :: World -> [Coordinate] -> Integer
count_active w cs = sum $ map (toInteger . fromEnum . w) cs

solve_2 = do
  w <- get_world
  cs <- get_starting_coordinates
  return $ uncurry count_active $ cycle_n_times 6 w cs






