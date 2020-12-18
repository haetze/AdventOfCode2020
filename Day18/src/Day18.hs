-- Created on 18 Dec 2020 by richard.stewing@udo.edu
-- Copyright Richard Stewing,18 Dec 2020
-- Licensed under GPLv3, See toplevel LICENSE file.

module Day18 where

import Prelude hiding (getChar, words)


import System.IO
import Control.Monad
import Text.Parsec.Prim as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Functor.Identity

inputF = "src/input.txt"

tailS [] = []
tailS (_:xs) = xs

next_exp :: String -> (String, String)
next_exp ('(':txt) = til_paren 0 txt
next_exp txt = (takeWhile (/=' ') txt, tailS $ dropWhile (/=' ') txt)

til_paren 0 (')':txt) = ("", dropWhile (==' ') txt)
til_paren n ('(':txt) = ('(':r, txt') where (r, txt') = til_paren (n+1) txt
til_paren n (')':txt) = (')':r, txt') where (r, txt') = til_paren (n-1) txt
til_paren n (c:txt)   = (c:r, txt') where (r, txt') = til_paren n txt


eval :: String -> Int
eval txt
  | not $ ' ' `elem` txt = read txt
  | otherwise            = eval_acc (eval next) rest where (next,rest) = next_exp txt

eval_acc acc [] = acc
eval_acc acc ('+':' ':txt) = eval_acc (acc + x) txt'
  where
    (next, txt') = next_exp txt
    x = eval next
eval_acc acc ('*':' ':txt) = eval_acc (acc * x) txt'
  where
    (next, txt') = next_exp txt
    x = eval next

solve_1 = readFile inputF >>= return . sum . map eval . lines


data Exp = Exp :+ Exp | Exp :* Exp | V Integer deriving(Show, Read, Eq)

language_def = emptyDef { Token.reservedOpNames = ["+", "*"]}


lexer = Token.makeTokenParser language_def

operators = [[Infix  (reserved_op "+"   >> return (:+)) AssocLeft]
            ,[Infix  (reserved_op "*"   >> return (:*)) AssocLeft]
            ]

reserved_op = Token.reservedOp lexer -- parses an operator
parens      = Token.parens     lexer -- parses surrounding parenthesis:
integer     = Token.integer    lexer -- parses an integer

parse_exp = buildExpressionParser operators parse_e

parse_e = choice $ map P.try [parens parse_exp, integer >>= return . V]

parse_string :: String -> Exp
parse_string str =
  case parse parse_exp "" str of
    Left e  -> error $ show e
    Right r -> r


eval_exp :: Exp -> Integer
eval_exp (V i) = i
eval_exp (e :+ e') = eval_exp e + eval_exp e'
eval_exp (e :* e') = eval_exp e * eval_exp e'

eval_s = eval_exp . parse_string


solve_2 = readFile inputF >>= return . sum . map eval_s . lines



