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


findCols :: [(Int, Int)] -> Int -> Int -> Window -> IO ()
findCols [] i j window = return ()
findCols (h : tail) i j window = do
  if snd h /= 0 then do
    drawPieces(i, j) (snd h) window
    findCols tail i (j + 1) window
  else findCols tail i (j + 1) window





boardBody :: Window -> IO ()
boardBody window = do
  (rows, cols) <- scrSize

  writeScreen (rows `div` 2 - 9) (cols `div` 2 - 18) " *━━━━━━━━━━━━━━━━━*━━━━━━━━━━━━━━━━━*" window
  writeScreen (rows `div` 2 - 8) (cols `div` 2 - 18) " ┃                 ┃                 ┃" window
  writeScreen (rows `div` 2 - 7) (cols `div` 2 - 18) " ┃                 ┃                 ┃" window
  writeScreen (rows `div` 2 - 6) (cols `div` 2 - 18) " ┃     *━━━━━━━━━━━*━━━━━━━━━━━*     ┃" window 
  writeScreen (rows `div` 2 - 5) (cols `div` 2 - 18) " ┃     ┃           ┃           ┃     ┃" window
  writeScreen (rows `div` 2 - 4) (cols `div` 2 - 18) " ┃     ┃           ┃           ┃     ┃" window
  writeScreen (rows `div` 2 - 3) (cols `div` 2 - 18) " ┃     ┃     *━━━━━*━━━━━*     ┃     ┃" window
  writeScreen (rows `div` 2 - 2) (cols `div` 2 - 18) " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen (rows `div` 2 - 1) (cols `div` 2 - 18) " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen (rows `div` 2 - 0) (cols `div` 2 - 18) " *━━━━━*━━━━━*           *━━━━━*━━━━━*" window
  writeScreen (rows `div` 2 + 1) (cols `div` 2 - 18) " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen (rows `div` 2 + 2) (cols `div` 2 - 18) " ┃     ┃     ┃           ┃     ┃     ┃" window
  writeScreen (rows `div` 2 + 3) (cols `div` 2 - 18) " ┃     ┃     *━━━━━*━━━━━*     ┃     ┃" window
  writeScreen (rows `div` 2 + 4) (cols `div` 2 - 18) " ┃     ┃           ┃           ┃     ┃" window
  writeScreen (rows `div` 2 + 5) (cols `div` 2 - 18) " ┃     ┃           ┃           ┃     ┃" window
  writeScreen (rows `div` 2 + 6) (cols `div` 2 - 18) " ┃     *━━━━━━━━━━━*━━━━━━━━━━━*     ┃" window
  writeScreen (rows `div` 2 + 7) (cols `div` 2 - 18) " ┃                 ┃                 ┃" window
  writeScreen (rows `div` 2 + 8) (cols `div` 2 - 18) " ┃                 ┃                 ┃" window
  writeScreen (rows `div` 2 + 9) (cols `div` 2 - 18) " *━━━━━━━━━━━━━━━━━*━━━━━━━━━━━━━━━━━*" window

