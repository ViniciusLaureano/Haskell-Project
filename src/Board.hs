module Board (boardGenerate) where

import UI.HSCurses.Curses

import WindowManipulation

boardGenerate :: (Int, Int) -> [[(Int, Int)]] -> Window -> IO ()
boardGenerate (y, x) matriz window = do
  drawBoard matriz window


drawBoard :: [[(Int, Int)]] -> Window -> IO ()
drawBoard matriz window = do
  (rows, cols) <- scrSize
  clearAndWriteScreen (rows `div` 2 - 11) (cols `div` 2 - 30) "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓" window
  writeScreen (rows `div` 2 + 14) (cols `div` 2 - 30) "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛" window
  desenhaVertical (cols `div` 2 - 30) window
  desenhaVertical (cols `div` 2 + 30) window

desenhaVertical :: Int -> Window -> IO()
desenhaVertical x window = sequence_ [mvWAddStr window y x "┃" | y <- [7..30]]
