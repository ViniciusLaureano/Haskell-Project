module Stage1Functions (mainLoop) where

import UI.HSCurses.Curses

import Board


-- Função para verificar se a posição é válida
isValidPosition :: [[(Int, Int)]] -> (Int, Int) -> Bool
isValidPosition matriz (r, c) =
  let (v, _) = matriz !! r !! c
  in v == 1

-- Função para checar se a posição está fora do tabuleiro
isOutOfBounds :: [[(Int, Int)]] -> (Int, Int) -> Bool
isOutOfBounds matriz (r, c) =
  r < 0 || r >= length matriz || c < 0 || c >= length (head matriz) || matriz !! r !! c == (-1, -1)

-- Função que encontra a próxima posição válida
findValidPosition :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findValidPosition matriz (r, c) (dr, dc)
  | isOutOfBounds matriz (r, c) = (r - dr, c - dc)  -- Para antes de sair
  | isValidPosition matriz (r, c) = (r, c)  -- Encontrou posição válida
  | otherwise = findValidPosition matriz (r + dr, c + dc) (dr, dc)  -- Continua movendo

-- Função para mover o cursor
moveCursor :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveCursor matriz (r, c) (dr, dc) =
  let newPos = findValidPosition matriz (r + dr, c + dc) (dr, dc)
  in if isValidPosition matriz newPos then newPos else (r, c)

-- Função para marcar uma posição no tabuleiro
markPosition :: [[(Int, Int)]] -> (Int, Int) -> [[(Int, Int)]]
markPosition matriz (r, c) =
  let (v, _) = matriz !! r !! c
  in if v == 1
     then take r matriz ++ [take c (matriz !! r) ++ [(1, 1)] ++ drop (c + 1) (matriz !! r)] ++ drop (r + 1) matriz
     else matriz


mainLoop :: [[(Int, Int)]] -> (Int, Int) -> Window -> IO ()
mainLoop matriz cursor window= do
  boardGenerate cursor matriz window
  ev <- getCh
  case ev of
    KeyChar 'w' -> mainLoop matriz (moveCursor matriz cursor (-1, 0)) window
    KeyChar 's' -> mainLoop matriz (moveCursor matriz cursor (1, 0)) window
    KeyChar 'a' -> mainLoop matriz (moveCursor matriz cursor (0, -1)) window
    KeyChar 'd' -> mainLoop matriz (moveCursor matriz cursor (0, 1)) window
    KeyChar 'm' -> mainLoop (markPosition matriz cursor) cursor window
    KeyChar 'q' -> return () 
    _ -> mainLoop matriz cursor window