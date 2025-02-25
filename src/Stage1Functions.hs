module Stage1Functions (readPiece, markPosition, mostraJogador) where

import UI.HSCurses.Curses

import Board
import WindowManipulation

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
markPosition :: [[(Int, Int)]] -> (Int, Int) -> Int -> [[(Int, Int)]]
markPosition matriz (r, c) player =
  let (v, _) = matriz !! r !! c
  in if v == 1
     then take r matriz ++ [take c (matriz !! r) ++ [(1, player)] ++ drop (c + 1) (matriz !! r)] ++ drop (r + 1) matriz
     else matriz


readPiece :: [[(Int, Int)]] -> (Int, Int) -> Window -> IO (Int, Int)
readPiece matriz cursor window = do
  boardGenerate cursor matriz window
  ev <- getCh
  case ev of
    KeyChar 'w' -> readPiece matriz (moveCursor matriz cursor (-1, 0)) window
    KeyChar 's' -> readPiece matriz (moveCursor matriz cursor (1, 0)) window
    KeyChar 'a' -> readPiece matriz (moveCursor matriz cursor (0, -1)) window
    KeyChar 'd' -> readPiece matriz (moveCursor matriz cursor (0, 1)) window
    KeyChar '\n' -> return cursor
    KeyChar 'q' -> return (-1, -1)
    _ -> readPiece matriz cursor window


mostraJogador :: Int -> String -> String -> Window -> IO ()
mostraJogador jogador nomeJogador1 nomeJogador2 window = 
  if jogador == 1 then 
    clearAndWriteScreen 0 0 nomeJogador1 window
  else
    clearAndWriteScreen 0 0 nomeJogador2 window