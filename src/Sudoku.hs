module Sudoku where

import           Control.Monad
import           Data.List
import           Debug.Trace

type Board = [[Int]]
type Pos = (Int, Int)

-- Try all numbers on an empty cell. 
-- If no luck, fail it.
solveSudoku :: Board -> Maybe Board
solveSudoku board
  | not $ validateBoard board = Nothing
  | isBoardFilled board = Just board
  | otherwise = do
    -- pick one empty cell
    let (i,j) = head $ getSortedEmptyCells board
    -- trace (getBoardView board) $ return ()

    -- try all numbers
    -- lazily get the first valid solution with msum
    msum $ [solveSudoku $ fillBoardNum i j num board | num <- [1..9]]

fillBoardNum :: Int -> Int -> Int -> Board -> Board
fillBoardNum i j num board = getBoardFromList $ xs1 ++ [num] ++ xs2
  where i' = i*9 + j
        (xs1, _:xs2) = splitAt i' $ getBoardNums board

isBoardFilled :: Board -> Bool
isBoardFilled board = notElem 0 $ getBoardNums board

-- 1. no duplicted number on each row & column
-- 2. no duplicated number in each 3x3 block
validateBoard :: Board -> Bool
validateBoard board =
  noDuplicatedNumInRows && noDuplicatedNumInCols && noDuplicatedNumInBlocks
 where
  noDuplicatedNumInRows   = all isUniqueList $ getBoardRows board
  noDuplicatedNumInCols   = all isUniqueList $ getBoardCols board
  noDuplicatedNumInBlocks = all isUniqueList $ getBoardBlocks board

isUniqueList :: [Int] -> Bool
isUniqueList xs = let xs' = filter (/= 0) xs in xs' == nub xs'

-- put empty cells with less choices first
getSortedEmptyCells :: Board -> [Pos]
getSortedEmptyCells board = sortOn f . getEmptyCells $ board
  where f (i,j) = sum $ map fromEnum $ map validateBoard $ [fillBoardNum i j num board | num <- [1..9]]

getEmptyCells :: Board -> [Pos]
getEmptyCells board = map (\i -> divMod i 9) idxs
  where idxs = elemIndices 0 $ getBoardNums board

getBoardNum :: Int -> Int -> Board -> Int
getBoardNum i j board = board !! i !! j

getBoardNums :: Board -> [Int]
getBoardNums = concat . getBoardRows

getBoardRows :: Board -> [[Int]]
getBoardRows = id

getBoardBlocks :: Board -> [[Int]]
getBoardBlocks board =
  [ [ getBoardNum (ii * 3 + 0) (jj * 3 + 0) board
    , getBoardNum (ii * 3 + 0) (jj * 3 + 1) board
    , getBoardNum (ii * 3 + 0) (jj * 3 + 2) board
    , getBoardNum (ii * 3 + 1) (jj * 3 + 0) board
    , getBoardNum (ii * 3 + 1) (jj * 3 + 1) board
    , getBoardNum (ii * 3 + 1) (jj * 3 + 2) board
    , getBoardNum (ii * 3 + 2) (jj * 3 + 0) board
    , getBoardNum (ii * 3 + 2) (jj * 3 + 1) board
    , getBoardNum (ii * 3 + 2) (jj * 3 + 2) board
    ]
  | ii <- [0 .. 2]
  , jj <- [0 .. 2]
  ]

getBoardCols :: Board -> [[Int]]
getBoardCols board =
  map (\j -> [ getBoardNum i j board | i <- [0 .. 8] ]) [0 .. 8]

getBoardView :: Board -> String
getBoardView board = 
  concat [ 

    (++ if (i == 2 || i == 5) then ("\n"++lineBreak++"\n") else "\n") $ concat [ 
      let num = getBoardNum i j board
          numStr = if num == 0 then " " else show num 
          separator = if (j == 2 || j == 5) then "|" else ""
      in numStr ++ separator
      | j <- [0..8]
    ] 
    | i <- [0..8]
  ]
  where
    lineBreak = replicate 11 '-'

drawBoard :: Board -> IO ()
drawBoard = putStrLn . getBoardView

getBoardFromList :: [a] -> [[a]]
getBoardFromList = chunks 9

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = (take n xs) : (chunks n (drop n xs))