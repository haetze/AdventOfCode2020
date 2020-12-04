-- Created on 04 Dec 2020 by richard.stewing@udo.edu
module Day4 where

import System.IO(readFile)

split_records :: String -> [String]
split_records txt = l_r:records where (l_r, records) = cut_on_blank_line txt

cut_on_blank_line :: String -> (String, [String])
cut_on_blank_line "" = ("", [])
cut_on_blank_line ('\n':'\n':txt) = ("", l_r:records) where (l_r, records) = cut_on_blank_line txt
cut_on_blank_line ('\n':txt) = (' ':l_r, records) where (l_r, records) = cut_on_blank_line txt
cut_on_blank_line (c:txt) = (c:l_r, records) where (l_r, records) = cut_on_blank_line txt


get_input :: IO [String]
get_input = do
  txt <- readFile "input.txt"
  return $ split_records txt


requested_fields :: [String]
requested_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validate_1 :: String -> Bool
validate_1 record = all (\field ->
                           any (\word ->
                                  take 3 word == field) (words record)) requested_fields

to_bin b = if b then 1 else 0

validate_2 :: String -> Bool
validate_2 record = all f requested_fields
  where
    f field = any (g field) (words record)
    g "byr" ('b':'y':'r':':':a:b:c:d:[]) = (a:b:c:d:[]) `elem` map show [1920..2002]
    g "iyr" ('i':'y':'r':':':a:b:c:d:[]) = (a:b:c:d:[]) `elem` map show [2010..2020]
    g "eyr" ('e':'y':'r':':':a:b:c:d:[]) = (a:b:c:d:[]) `elem` map show [2020..2030]
    g "hgt" ('h':'g':'t':':':a:b:'i':'n':[]) = (a:b:[]) `elem` map show [59..76]
    g "hgt" ('h':'g':'t':':':a:b:c:'c':'m':[]) = (a:b:c:[]) `elem` map show [150..193]
    g "hcl" ('h':'c':'l':':':'#':a:b:c:d:e:f:[]) = all (\i ->
                                                          i `elem` ['0'..'9'] ++ ['a'..'f'])
                                                   (a:b:c:d:e:f:[])
    g "ecl" ('e':'c':'l':':':col) = col `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    g "pid" ('p':'i':'d':':':a:b:c:d:e:f:g:h:i:[]) = all (\j ->
                                                            j `elem` ['0'..'9'])
                                                     (a:b:c:d:e:f:g:h:i:[])
    g _ _ = False


solve f = do
  records <- get_input
  return $ sum $ map (to_bin . f) records

solve_1 = solve validate_1
solve_2 = solve validate_2
