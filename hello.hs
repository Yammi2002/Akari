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
  take y board
    ++ [placeLampadina x (board !! y)]
    ++ drop (y + 1) board

placeLampadina :: Int -> [Cella] -> [Cella]
placeLampadina i row =
  take i row ++ [Lampadina] ++ drop (i + 1) row
