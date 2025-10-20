module Main where

import Akari
import Control.Monad (when)

applyMoves :: Board -> [(Int, Int)] -> Board
applyMoves = foldl (\b (x, y) -> illuminateBoard (toggleLampadina x y b))

main :: IO ()
main = do
  putStrLn "=== AKARI Puzzle ==="
  content <- readFile "board/board.7x7.1"
  let initialBoard = illuminateBoard (parseBoard content)
  putStrLn "\nBoard iniziale:"
  loop initialBoard []

loop :: Board -> [(Int, Int)] -> IO ()
loop initialBoard moves = do
  let board = applyMoves initialBoard moves
  putStrLn (boardToString (illuminateBoard board))
  if isComplete board
    then putStrLn "*** Gioco completato! Complimenti! ***"
    else do
      putStrLn "Inserisci mossa (x y) oppure 'q' per uscire:"
      line <- getLine
      when (not $ null line) $ do
        if line == "q"
          then putStrLn "*** Gioco terminato. ***"
          else do
            let [sx, sy] = words line
                x = read sx
                y = read sy
            loop initialBoard (moves ++ [(x, y)])
