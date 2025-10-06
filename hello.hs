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