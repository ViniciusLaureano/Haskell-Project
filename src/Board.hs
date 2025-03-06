module Board (boardGenerate, boardHistory) where

import UI.HSCurses.Curses

import Control.Monad (forM_)

import WindowManipulation


boardGenerate :: (Int, Int) -> [[(Int, Int)]] -> Window -> IO ()
boardGenerate (y, x) matrix window = do
  boardBody window
  findPieces matrix 1 1 window
  drawPointer (y, x) window


drawPointer :: (Int, Int) -> Window -> IO ()
drawPointer (y, x) window = do
  (rows, cols) <- scrSize
  writeScreen ((y - 1) * 3 + (rows `div` 2 - 9)) ((x - 1) * 6 + (cols `div` 2 - 17)) "X" window

drawPieces :: (Int, Int) -> Int -> Window -> IO ()
drawPieces (y, x) player window = do
  (rows, cols) <- scrSize
  
  if player == 1 then 
    writeScreen ((y - 2) * 3 + (rows `div` 2 - 9)) ((x - 2) * 6 + (cols `div` 2 - 17)) "1" window
  else 
    if player == 2 then
      writeScreen ((y - 2) * 3 + (rows `div` 2 - 9)) ((x - 2) * 6 + (cols `div` 2 - 17)) "2" window
    else
      return ()


findPieces :: [[(Int, Int)]] -> Int -> Int -> Window -> IO ()
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



boardHistory :: [[(Int, Int)]] -> Window -> IO ()
boardHistory matrix window = do
  boardBody window
  findPieces matrix 1 1 window