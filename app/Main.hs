module Main where

import           Sudoku
import           Constant

main :: IO ()
main = do
  let sudoku = exampleBoard2
  askSudoku sudoku
  -- sudoku <- randomSudokuExercise
  -- return ()

askSudoku :: [[Int]] -> IO ()
askSudoku sudoku = do
  -- let sudoku = getBoardFromList nums
  putStrLn "--- Sudoku Solver ---"
  putStrLn "Solving example sudoku:"
  drawBoard sudoku
  putStrLn "Calculating..."
  case solveSudoku sudoku of
    Just solution -> do
      putStrLn "\n--- Solution ---\n"
      drawBoard solution
    Nothing -> putStrLn "No solution!"
