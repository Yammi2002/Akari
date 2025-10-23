module Main where

import Akari

applyMove :: Board -> (Int, Int) -> Board
applyMove b (x, y) = illuminateBoard (toggleLampadina x y b)

parseMove :: String -> (Int, Int)
parseMove line =
  let [sx, sy] = words line
  in (read sx, read sy)

main :: IO ()
main = do
  putStrLn "=== AKARI Puzzle ==="
  content <- readFile "boards/board7x7.1"
  let initialBoard = illuminateBoard (parseBoard content)

  putStrLn "\nBoard iniziale:"
  putStrLn (boardToString initialBoard)

  putStrLn "\nInserisci mosse (x y), una per riga. Digita Ctrl+D (Unix) o Ctrl+Z (Windows) per terminare.\n"

  input <- getContents

  let moves = map parseMove (lines input)

      states = scanl applyMove initialBoard moves

      playing = takeWhile (not . isComplete) states

  mapM_ (\b -> do
           putStrLn "\nBoard aggiornata:"
           putStrLn (boardToString b)
        ) playing

  putStrLn "\n*** Gioco terminato o completato! ***"
