module Main where

import Control.DeepSeq
import Control.Exception -- evaluate
import Control.Parallel.Strategies
import Data.Maybe
import Sudoku
import System.Environment

-- Expected cmd line args:
--   filename version
-- The filename containing the puzzles.
-- The version is one of (v1, v2, v3, v4).
main :: IO ()
main = do
  f : v : [] <- getArgs
  file       <- readFile f
  let puzzles   = lines file
      solutions = version v puzzles
  if v == "v4" then evaluate (length puzzles) else return 0
  print (length (filter isJust solutions))

version :: String -> [String] -> [Maybe Sudoku.GridValues]
version "v1" = v1
version "v2" = v2
version "v3" = v3
version "v4" = v3

v1 :: [String] -> [Maybe Sudoku.GridValues]
v1 puzzles = map solve puzzles

v2 :: [String] -> [Maybe Sudoku.GridValues]
v2 puzzles =
  let (as,bs) = splitAt (length puzzles `div` 2) puzzles in
    runEval $ do
        as' <- rpar (force (map solve as))
        bs' <- rpar (force (map solve bs))
        rseq as'
        rseq bs'
        return (as' ++ bs')

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
  b  <- rpar (f a)
  bs <- parMap' f as
  return (b:bs)

v3 :: [String] -> [Maybe Sudoku.GridValues]
v3 puzzles = runEval (parMap' solve puzzles)
