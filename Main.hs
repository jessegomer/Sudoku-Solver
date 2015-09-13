module Main where

import Control.Monad
import Sudoku

main :: IO ()
main = putStrLn prompt >> getLine >>= readFile >>=  (putStrLn . process)


prompt = "Please enter file name"

process :: String -> String
process b = let completed = toBoard b >>= dfsSolve in
              maybe "Invalid board" ppBoard completed
