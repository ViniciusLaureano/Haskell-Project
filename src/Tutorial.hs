module Tutorial (startTutorial) where

import UI.HSCurses.Curses

import WindowManipulation

import Board

import GameComponents

matriz = matrizDefault

startTutorial :: Window -> IO()
startTutorial window = do
  clearAndWriteScreen 4 60 "Esse será o seu tabuleiro durante o jogo se familiarize com ele :) " window
  writeScreen 11 100 "➡ para avançar " window
  boardGenerate (2, 4) matriz window
  key <- getCh
  case key of
    KeyRight -> upMove matriz window (2, 4)
    KeyChar 'q' -> return() 
    _ -> startTutorial window
    

upMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
upMove matriz window (y, x) = do
  clearAndWriteScreen 4 60 "W A S D será a sua movimentação durante o jogo" window
  writeScreen 10 60 "Teste apertando W pra mover o cursor para cima" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'w' -> upMove matriz window (1, 4)
    KeyRight -> rightMove matriz window (1, 4)
    KeyLeft -> startTutorial window
    KeyChar 'q' -> return ()
    _ -> upMove matriz window (y, x)

rightMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
rightMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte D para mover o cursor para a direita" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'd' -> rightMove matriz window (1, 7)
    KeyRight -> leftMove matriz window (1, 7)
    KeyLeft -> upMove matriz window (2, 4)
    KeyChar 'q' -> return ()
    _ -> rightMove matriz window (y, x)

leftMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
leftMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte A para mover o cursor para a esquerda" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'a' -> leftMove matriz window (1, 4)
    KeyRight -> downMove matriz window (1, 4)
    KeyLeft -> rightMove matriz window (1, 4)
    KeyChar 'q' -> return ()
    _ -> leftMove matriz window (y, x)
 
downMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
downMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte S para mover o cursor para baixo" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 's' -> downMove matriz window (2, 4)
    KeyRight -> makeMove matriz window (2, 4)
    KeyLeft -> leftMove matriz window (1, 7)
    KeyChar 'q' -> return ()
    _ -> downMove matriz window (y, x)

makeMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
makeMove matriz window (y, x)= do
  clearAndWriteScreen 10 60 "Aperte ENTER para colocar uma peça na posição do cursor cursor" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  boardGenerate (2, 4) matriz window
  key <- getCh
  case key of
    KeyChar '\n' -> makeMove matriz window (1, 4)
    KeyLeft -> downMove matriz window (1, 4)
    _ -> return ()
