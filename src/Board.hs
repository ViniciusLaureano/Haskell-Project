module Board (boardGenerate, drawBoard, drawPointer) where

import UI.HSCurses.Curses

import Control.Monad (forM_)

import WindowManipulation


boardGenerate :: (Int, Int) -> [[(Int, Int)]] -> Window -> IO ()
boardGenerate (y, x) matriz window = do
  drawBoard matriz window
  findPieces matriz 1 1 window
  drawPointer (y, x) window



drawBoard :: [[(Int, Int)]] -> Window -> IO ()
drawBoard matriz window = boardBody window


drawPointer :: (Int, Int) -> Window -> IO()
-- drawPointer (y, x) window = writeScreen 14 60  "X" window
drawPointer (y, x) window = writeScreen line cols "X" window
  where
    line = (y * 3 + 14)
    cols = (x * 6 + 60)

drawPieces :: (Int, Int) -> Int -> Window -> IO()
drawPieces (y, x) player window = do
  if player == 1 then writeScreen line cols "1" window
  else writeScreen y x "2" window
  where
    line = (y * 3 + 14)
    cols = (x * 6 + 60)


findPieces :: [[(Int, Int)]] -> Int -> Int -> Window -> IO()
findPieces [] i j window = return ()
findPieces (h : tail) i j window = do
  findCols h i j window
  findPieces tail (i + 1) j window


findCols :: [(Int, Int)] -> Int -> Int -> Window -> IO()
findCols [] i j window = return ()
findCols (h : tail) i j window = do
  if snd h /= 0 then do
    drawPieces(i, j) (snd h) window
    findCols tail i (j + 1) window
  else findCols tail i (j + 1) window





boardBody :: Window -> IO()
boardBody window = do
  writeScreen 14 60 " *━━━━━━━━━━━━━━━━━*━━━━━━━━━━━━━━━━━*" window
  writeScreen 15 60 " ┃                 ┃                 ┃" window
  writeScreen 16 60 " ┃                 ┃                 ┃" window
  writeScreen 17 60 " ┃     *━━━━━━━━━━━*━━━━━━━━━━━*     ┃" window 
  writeScreen 18 60 " ┃     ┃           ┃           ┃     ┃" window
  writeScreen 19 60 " ┃     ┃           ┃           ┃     ┃" window
  writeScreen 20 60 " ┃     ┃     *━━━━━*━━━━━*     ┃     ┃" window
  writeScreen 21 60 " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen 22 60 " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen 23 60 " *━━━━━*━━━━━*           *━━━━━*━━━━━*" window
  writeScreen 24 60 " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen 25 60 " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen 26 60 " ┃     ┃     *━━━━━*━━━━━*     ┃     ┃" window
  writeScreen 27 60 " ┃     ┃           ┃           ┃     ┃" window
  writeScreen 28 60 " ┃     ┃           ┃           ┃     ┃" window
  writeScreen 29 60 " ┃     *━━━━━━━━━━━*━━━━━━━━━━━*     ┃" window
  writeScreen 30 60 " ┃                 ┃                 ┃" window
  writeScreen 31 60 " ┃                 ┃                 ┃" window
  writeScreen 32 60 " *━━━━━━━━━━━━━━━━━*━━━━━━━━━━━━━━━━━*" window
