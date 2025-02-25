module Stage1Functions (mainLoop) where

import UI.HSCurses.Curses

import WindowManipulation

import GameComponents

import Control.Monad (when)

import Board

import Validations

--Função para desenhar o tabuleiro
drawBoard :: [[(Int, Int)]] -> Window -> IO ()
drawBoard board window = do
  wclear window
  mapM_ (uncurry drawRow) (zip [0..] board)
  wRefresh window
  where
    drawRow r row = mapM_ (uncurry (drawCell r)) (zip [0..] row)
    drawCell r c (-1, -1) = mvWAddStr window r c "#"
    drawCell r c (1, 0)   = mvWAddStr window r c " "
    drawCell r c (1, 1)   = mvWAddStr window r c "●"
    drawCell r c (0, 0)   = return ()  -- Espaço inválido não desenhado

-- Função para verificar se a posição é válida
isValidPosition :: [[(Int, Int)]] -> (Int, Int) -> Bool
isValidPosition board (r, c) =
  let (v, _) = board !! r !! c
  in v == 1

-- Função para checar se a posição está fora do tabuleiro
isOutOfBounds :: [[(Int, Int)]] -> (Int, Int) -> Bool
isOutOfBounds board (r, c) =
  r < 0 || r >= length board || c < 0 || c >= length (head board) || board !! r !! c == (-1, -1)

-- Função que encontra a próxima posição válida
findValidPosition :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findValidPosition board (r, c) (dr, dc)
  | isOutOfBounds board (r, c) = (r - dr, c - dc)  -- Para antes de sair
  | isValidPosition board (r, c) = (r, c)  -- Encontrou posição válida
  | otherwise = findValidPosition board (r + dr, c + dc) (dr, dc)  -- Continua movendo

-- Função para mover o cursor
moveCursor :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveCursor board (r, c) (dr, dc) =
  let newPos = findValidPosition board (r + dr, c + dc) (dr, dc)
  in if isValidPosition board newPos then newPos else (r, c)

-- Função para marcar uma posição no tabuleiro
markPosition :: [[(Int, Int)]] -> (Int, Int) -> [[(Int, Int)]]
markPosition board (r, c) =
  let (v, _) = board !! r !! c
  in if v == 1
     then take r board ++ [take c (board !! r) ++ [(1, 1)] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board
     else board


mainLoop :: [[(Int, Int)]] -> (Int, Int) -> Window -> IO ()
mainLoop board cursor window= do
  drawBoard board window
  mvWAddStr window (fst cursor) (snd cursor) "X"
  wRefresh window
  ev <- getCh
  case ev of
    KeyChar 'w' -> mainLoop board (moveCursor board cursor (-1, 0)) window
    KeyChar 's' -> mainLoop board (moveCursor board cursor (1, 0)) window
    KeyChar 'a' -> mainLoop board (moveCursor board cursor (0, -1)) window
    KeyChar 'd' -> mainLoop board (moveCursor board cursor (0, 1)) window
    KeyChar 'm' -> mainLoop (markPosition board cursor) cursor window
    KeyChar 'q' -> return () 
    _ -> mainLoop board cursor window