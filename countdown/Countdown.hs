--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 22: Fun with sequential composition                                --
--------------------------------------------------------------------------------
-- The code in this file is based on Graham Hutton's code for solving the     --
-- countdown problem.                                                         --
--------------------------------------------------------------------------------

module Countdown where

--------------------------------------------------------------------------------

import Control.Monad (guard)
import Data.List (permutations, subsequences)

--------------------------------------------------------------------------------

data Op   = Add | Sub | Mul | Div deriving Show
data Expr = Val Integer | App Op Expr Expr deriving Show

apply :: Op -> Integer -> Integer -> Maybe Integer
apply Add x y = return (x + y)
apply Sub x y = return (x - y)
apply Mul x y = return (x * y)
apply Div x 0 = Nothing
apply Div x y = return (x `div` y)

eval :: Expr -> Maybe Integer
eval (Val n) = do
    guard (n > 0)
    return n
eval (App o l r) = do
    x <- eval l
    y <- eval r
    apply o x y

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

choices :: [a] -> [[a]]
choices = concat . map permutations . subsequences

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Integer] -> [Expr]
exprs []  = []
exprs [n] = return (Val n)
exprs ns  = do
    (ls,rs) <- split ns
    l       <- exprs ls
    r       <- exprs rs
    e       <- combine l r
    return e

combine :: Expr -> Expr -> [Expr]
combine l r = do
    o <- ops
    return $ App o l r

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = do
    ns' <- choices ns
    e   <- exprs ns'
    guard (eval e == Just n)
    return e

--------------------------------------------------------------------------------
