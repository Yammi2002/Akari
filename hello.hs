import Data.List (transpose)
-- Data definitions

data Cella
    = Vuota             -- Cella bianca non illuminata
    | Illuminata Int    -- Illuminata da N lampadine
    | Lampadina         -- C'è una lampadina
    | Nera (Maybe Int)  -- Blocco nero, con o senza numero (0..4)
    deriving (Show, Eq)

type Board = [[Cella]]

-- Char parser

charToCella :: Char -> Cella
charToCella '.' = Vuota
charToCella '*' = Lampadina
charToCella '#' = Nera Nothing
charToCella '0' = Nera (Just 0)
charToCella '1' = Nera (Just 1)
charToCella '2' = Nera (Just 2)
charToCella '3' = Nera (Just 3)
charToCella '4' = Nera (Just 4)
charToCella _   = error "Carattere non valido"

-- Funzione per piazzare una lampadina

play :: Int -> Int -> Board -> Board
play x y board =
  let board'  = replaceAt y (illuminateRow x (board !! y)) board
      board'' = illuminateColumn x y board'
  in board''

placeLampadina :: Int -> [Cella] -> [Cella]
placeLampadina i row =
  take i row ++ [Lampadina] ++ drop (i + 1) row

scanWhileLightable :: (Cella -> Cella) -> [Cella] -> [Cella]
scanWhileLightable f = go True
  where
    go _ [] = []
    go False xs = xs  -- fermati
    go True (c:cs)
      | isLightable c = f c : go True cs
      | otherwise     = c : go False cs

illuminateRow :: Int -> [Cella] -> [Cella]
illuminateRow x row =
  let (left, _:right) = splitAt x row
      left'  = reverse $ scanWhileLightable illumina $ reverse left
      right' = scanWhileLightable illumina right
  in left' ++ [Lampadina] ++ right'

isLightable :: Cella -> Bool
isLightable Vuota = True
isLightable (Illuminata _) = True
isLightable _ = False

illumina :: Cella -> Cella
illumina Vuota = Illuminata 1
illumina (Illuminata n) = Illuminata (n + 1)
illumina c = c

illuminateColumn :: Int -> Int -> Board -> Board
illuminateColumn x y board =
  transpose $ replaceAt x newCol (transpose board)
  where
    col = (transpose board) !! x
    newCol = illuminateRow y col

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs

--codice aggiunto da Luca
--questa funzione restituisce il risultato se vinto o ancora incompleto
isComplete :: Board -> Bool
isComplete board =
    allCellsIlluminated board && allWallsSatisfied board
--controlla se nella mappa è presente una cella vuota
allCellsIlluminated :: Board -> Bool
allCellsIlluminated = all (all isIlluminatedOrBlocked)
  where
    isIlluminatedOrBlocked (Illuminata _) = True
    isIlluminatedOrBlocked Lampadina      = True
    isIlluminatedOrBlocked (Nera _)       = True
    isIlluminatedOrBlocked _              = False
--conta le luci attaccate al muro
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
--controlla che tutti i muri siano con il numero corretto di luci
allWallsSatisfied :: Board -> Bool
allWallsSatisfied board =
    all checkCell [(y,x) | y <- [0..rows-1], x <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    checkCell (y,x) = case (board !! y) !! x of
        Nera (Just n) -> countAdjacentLamps board y x == n
        _             -> True