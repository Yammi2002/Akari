module Akari where

import Foreign.C.String (CString, peekCString, newCString)

-- Esporta funzioni WASM
foreign export ccall play_wasm :: CString -> Int -> Int -> IO CString
foreign export ccall isComplete_wasm :: CString -> IO Int

play_wasm :: CString -> Int -> Int -> IO CString
play_wasm cstr x y = do
  input <- peekCString cstr
  let board = parseBoard input
  let newBoard = toggleLampadina x y board
  let illuminatedBoard = illuminateBoard newBoard
  let outStr = boardToString illuminatedBoard
  newCString outStr

isComplete_wasm :: CString -> IO Int
isComplete_wasm cstr = do
  input <- peekCString cstr
  let board = parseBoard input
  let res = if isComplete board then 1 else 0
  return res

-- Data definitions

data Cella
    = Vuota             -- Cella bianca non illuminata
    | Illuminata Int    -- Illuminata da N lampadine
    | Lampadina         -- C'è una lampadina
    | Nera (Maybe Int)  -- Blocco nero, con o senza numero (0..4)
    deriving (Show, Eq)

type Board = [[Cella]]

-- Parser / Serializzazione

charToCella :: Char -> Cella
charToCella '.' = Vuota
charToCella '*' = Lampadina
charToCella '+' = Illuminata 1
charToCella 'a' = Illuminata 2
charToCella 'b' = Illuminata 3
charToCella 'c' = Illuminata 4
charToCella '5' = Illuminata 5
charToCella '6' = Illuminata 6
charToCella '7' = Illuminata 7
charToCella '8' = Illuminata 8
charToCella '9' = Illuminata 9
charToCella '#' = Nera Nothing
charToCella '0' = Nera (Just 0)
charToCella '1' = Nera (Just 1)
charToCella '2' = Nera (Just 2)
charToCella '3' = Nera (Just 3)
charToCella '4' = Nera (Just 4)
charToCella c   = error $ "Carattere non valido: " ++ [c]

cellaToChar :: Cella -> Char
cellaToChar Vuota          = '.'
cellaToChar Lampadina      = '*'
cellaToChar (Illuminata 1) = '+'
cellaToChar (Illuminata 2) = 'a'
cellaToChar (Illuminata 3) = 'b'
cellaToChar (Illuminata 4) = 'c'
cellaToChar (Illuminata n)
  | n >= 5 && n <= 9       = head (show n)
  | otherwise              = 'x'  -- valore imprevisto
cellaToChar (Nera Nothing) = '#'
cellaToChar (Nera (Just n)) = head (show n)

parseBoard :: String -> Board
parseBoard = map (map charToCella) . lines

boardToString :: Board -> String
boardToString = unlines . map (map cellaToChar)

-- Gestione gioco

toggleLampadina :: Int -> Int -> Board -> Board
toggleLampadina x y board =
  case (board !! y) !! x of
    Lampadina -> removeLampadina x y board
    Vuota     -> placeLampadina x y board
    Illuminata _ -> placeLampadina x y board -- permette piazzare se illuminata ma vuota
    _         -> board -- nessuna azione su muri neri

placeLampadina :: Int -> Int -> Board -> Board
placeLampadina x y board = replaceAt y newRow board
  where
    row = board !! y
    newRow = replaceAt x Lampadina row

removeLampadina :: Int -> Int -> Board -> Board
removeLampadina x y board = replaceAt y newRow board
  where
    row = board !! y
    newRow = replaceAt x Vuota row

illuminateBoard :: Board -> Board
illuminateBoard board =
  let boardNoLight = clearIllumination board
      lampPositions = [ (x,y) | y <- [0..length board -1], x <- [0..length (head board) -1], (boardNoLight !! y) !! x == Lampadina ]
  in foldl illuminateFromLamp boardNoLight lampPositions

clearIllumination :: Board -> Board
clearIllumination = map (map clearCell)
  where
    clearCell (Illuminata _) = Vuota
    clearCell c = c

illuminateFromLamp :: Board -> (Int, Int) -> Board
illuminateFromLamp board (x,y) =
  let board' = illuminateLine x y board (1, 0)   -- destra
      board'' = illuminateLine x y board' (-1,0) -- sinistra
      board''' = illuminateLine x y board'' (0, 1) -- giù
      board'''' = illuminateLine x y board''' (0, -1) -- su
  in board''''

illuminateLine :: Int -> Int -> Board -> (Int, Int) -> Board
illuminateLine x y board (dx, dy) = go (x+dx) (y+dy) board
  where
    go cx cy b
      | not (inBounds cx cy b) = b
      | otherwise =
          case (b !! cy) !! cx of
            Vuota -> go (cx+dx) (cy+dy) (replaceCell cx cy (Illuminata 1) b)
            Illuminata n -> go (cx+dx) (cy+dy) (replaceCell cx cy (Illuminata (n+1)) b)
            _ -> b -- muro o lampadina, fermati
    inBounds ix iy brd = iy >= 0 && iy < length brd && ix >= 0 && ix < length (head brd)

replaceCell :: Int -> Int -> Cella -> Board -> Board
replaceCell x y newCell board =
  replaceAt y newRow board
  where
    row = board !! y
    newRow = replaceAt x newCell row

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs = take i xs ++ [val] ++ drop (i+1) xs

isComplete :: Board -> Bool
isComplete board =
  allCellsIlluminated board && allWallsSatisfied board

allCellsIlluminated :: Board -> Bool
allCellsIlluminated = all (all isIlluminatedOrBlocked)
  where
    isIlluminatedOrBlocked (Illuminata _) = True
    isIlluminatedOrBlocked Lampadina      = True
    isIlluminatedOrBlocked (Nera _)       = True
    isIlluminatedOrBlocked _              = False

countAdjacentLamps :: Board -> Int -> Int -> Int
countAdjacentLamps board y x =
  length [ ()
         | (dy, dx) <- [(-1,0),(1,0),(0,-1),(0,1)]
         , let ny = y + dy
         , let nx = x + dx
         , inBounds ny nx
         , (board !! ny) !! nx == Lampadina
         ]
  where
    inBounds i j = i >= 0 && j >= 0 && i < length board && j < length (head board)

allWallsSatisfied :: Board -> Bool
allWallsSatisfied board =
  all checkCell [(y,x) | y <- [0..rows-1], x <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    checkCell (y,x) = case (board !! y) !! x of
      Nera (Just n) -> countAdjacentLamps board y x == n
      _             -> True
