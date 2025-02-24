module MenuFunctionalities (newGame, continueGame, matchHistory) where

import UI.HSCurses.Curses

import WindowManipulation

import Board 


newGame :: Window -> IO ()
newGame window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

continueGame :: Window -> IO ()
continueGame window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

matchHistory :: Window -> IO ()
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window
