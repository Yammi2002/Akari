module Exports where
import Akari

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