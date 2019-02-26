--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 22: Fun with sequential composition                                --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (replicateM)
import Control.Monad.Trans.State

import System.Random

import Countdown

--------------------------------------------------------------------------------

runRandom :: State StdGen a -> Int -> a
runRandom m n = fst $ runState m (mkStdGen n)

randomNumber :: State StdGen Int
randomNumber = state random

--------------------------------------------------------------------------------

data Problem = Problem {
    problemTarget :: Integer,
    problemValues :: [Integer]
}

p0 :: Problem
p0 = Problem 4 [1,2,3]

p1 :: Problem
p1 = Problem 23 [4,8,15,16]

problems :: [Problem]
problems = [p0,p1]

solveRandomProblems :: Int -> [Problem] -> State StdGen [Expr]
solveRandomProblems n ps = undefined

--------------------------------------------------------------------------------

run :: Int -> Int -> IO ()
run x y = mapM_ print es
    where es = runRandom (solveRandomProblems x problems) y

main :: IO ()
main = return ()

--------------------------------------------------------------------------------
