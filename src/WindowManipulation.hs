module WindowManipulation (writeScreen, clearAndWriteScreen) where

import UI.HSCurses.Curses

writeScreen :: Int -> Int -> String -> Window -> IO ()
writeScreen y x title window = do 
  mvWAddStr window y x title
  wRefresh window

clearAndWriteScreen :: Int -> Int -> String -> Window -> IO ()
clearAndWriteScreen y x title window = do 
  wclear window
  writeScreen y x title window