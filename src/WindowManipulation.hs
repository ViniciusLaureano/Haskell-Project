module WindowManipulation (writeScreen, clearAndWriteScreen, writeScreenCenter, clearAndWriteScreenCenter) where

import UI.HSCurses.Curses

writeScreen :: Int -> Int -> String -> Window -> IO ()
writeScreen y x title window = do 
  mvWAddStr window y x title
  wRefresh window

clearAndWriteScreen :: Int -> Int -> String -> Window -> IO ()
clearAndWriteScreen y x title window = do 
  wclear window
  writeScreen y x title window

writeScreenCenter :: Int -> String -> Window -> IO ()
writeScreenCenter y message window = do
  (_, cols) <- scrSize
  let x = (cols - length message) `div` 2
  writeScreen y x message window 

clearAndWriteScreenCenter :: Int -> String -> Window -> IO ()
clearAndWriteScreenCenter y message window = do
  wclear window
  writeScreenCenter y message window