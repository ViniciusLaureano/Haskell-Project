module Tutorial (startTutorial) where

import UI.HSCurses.Curses

import WindowManipulation

import Board

import GameComponents

matriz = matrizDefault

startTutorial :: Window -> IO()
startTutorial window = do
  boardGenerate (2, 4) matriz window
  upMove matriz window

upMove :: [[(Int, Int)]] -> Window -> IO()
upMove matriz window = do
  key <- getCh
  case key of
      KeyChar 'w' -> rightMove matriz window
      _ -> upMove matriz window

rightMove :: [[(Int, Int)]] -> Window -> IO()
rightMove matriz window = do
  boardGenerate (1, 4) matriz window
  key <- getCh
  case key of
    KeyChar 'd' -> leftMove matriz window
    _ -> rightMove matriz window

leftMove :: [[(Int, Int)]] -> Window -> IO()
leftMove matriz window = do
  boardGenerate (1, 7) matriz window
  key <- getCh
  case key of
    KeyChar 'a' -> downMove matriz window
    _ -> leftMove matriz window
 
downMove :: [[(Int, Int)]] -> Window -> IO()
downMove matriz window = do
  boardGenerate (1, 4) matriz window
  key <- getCh
  case key of
    KeyChar 's' -> makeMove matriz window
    _ -> downMove matriz window

makeMove :: [[(Int, Int)]] -> Window -> IO()
makeMove matriz window = do
  boardGenerate (2, 4) matriz window
  key <- getCh
  case key of
    KeyChar '\n' -> makeMove matriz window
