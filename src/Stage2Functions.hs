module Stage2Functions (readMove, movePiece, handleMillRemoval) where

import UI.HSCurses.Curses

import Board
import WindowManipulation

import Stage1Functions

-- Retorna as posições válidas para mover uma peça específica
getValidMoves :: [[(Int, Int)]] -> (Int, Int) -> [(Int, Int)]
getValidMoves board (r, c) =
  let adjacentes = getValidConnections (r, c)
  in filter (\pos -> isValidPosition board pos && snd (board !! fst pos !! snd pos) == 0) adjacentes

getValidConnections :: (Int, Int) -> [(Int, Int)]
getValidConnections (r, c) = case (r, c) of
  (1,1) -> [(1,4), (4,1)]
  (1,4) -> [(1,1), (1,7), (2,4)]
  (1,7) -> [(1,4), (4,7)]
  (2,4) -> [(1,4), (2,2), (2,6), (3,4)]
  (2,2) -> [(2,4), (4,2)]
  (2,6) -> [(2,4), (4,6)]
  (3,4) -> [(3,3), (2,4), (3,5)]
  (3,3) -> [(3,4), (4,3)]
  (3,5) -> [(3,4), (4,5)]
  (4,1) -> [(1,1), (7,1), (4,2)]
  (4,2) -> [(2,2), (4,1), (6,2), (4,3)]
  (4,3) -> [(3,3), (4,2), (5,3)]
  (4,5) -> [(3,5), (4,6), (5,5)]
  (4,6) -> [(4,5), (4,7), (2,6), (6,6)]
  (4,7) -> [(1,7), (4,6), (7,7)]
  (5,3) -> [(4,3), (5,4)]
  (5,4) -> [(5,3), (6,4), (5,5)]
  (5,5) -> [(5,4), (4,5)]
  (6,2) -> [(4,2), (6,4)]
  (6,4) -> [(6,2), (5,4), (6,6),(7,4)]
  (6,6) -> [(4,6), (6,4)]
  (7,1) -> [(4,1), (7,4)]
  (7,4) -> [(7,1), (6,4), (7,7)]
  (7,7) -> [(4,7), (7,4)]
  _ -> []

-- Move uma peça de um jogador para uma nova posição válida
movePiece :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> Int -> [[(Int, Int)]]
movePiece board (r1, c1) (r2, c2) jogador =
  let boardSemPeca = take r1 board ++ [take c1 (board !! r1) ++ [(1, 0)] ++ drop (c1 + 1) (board !! r1)] ++ drop (r1 + 1) board
      boardComPeca = take r2 boardSemPeca ++ [take c2 (boardSemPeca !! r2) ++ [(1, jogador)] ++ drop (c2 + 1) (boardSemPeca !! r2)] ++ drop (r2 + 1) boardSemPeca
  in boardComPeca

-- Executa a lógica de remoção de peça quando um moinho é formado
handleMillRemoval :: [[(Int, Int)]] -> Int -> Window -> IO ([[ (Int, Int) ]], Bool)
handleMillRemoval board jogador window = do
  let oponente = if jogador == 1 then 2 else 1
  let todasPecasOponente = [(r', c') | r' <- [0..7], c' <- [0..7], snd (board !! r' !! c') == oponente]
  let pecasNaoMoinho = filter (\pos -> not (isMillFormed board pos oponente)) todasPecasOponente

  posToRemove <- if null pecasNaoMoinho  
                then selectOpponentPiece board oponente window
                else selectOpponentPieceFromList board oponente pecasNaoMoinho window

  if posToRemove == (-1, -1)
    then return (board, True)  
    else do
      let updatedBoard = removeOpponentPiece board posToRemove
      return (updatedBoard, True)  

-- Permite que o jogador selecione uma peça e escolha um movimento válido
readMove :: [[(Int, Int)]] -> (Int, Int) -> Int -> Window -> IO (Int, Int, Int, Int)
readMove board cursor jogador window = do
  boardGenerate cursor board window
  ev <- getCh
  case ev of
    KeyChar 'w' -> readMove board (moveCursor board cursor (-1, 0)) jogador window
    KeyChar 's' -> readMove board (moveCursor board cursor (1, 0)) jogador window
    KeyChar 'a' -> readMove board (moveCursor board cursor (0, -1)) jogador window
    KeyChar 'd' -> readMove board (moveCursor board cursor (0, 1)) jogador window
    KeyChar '\n' -> do
      if snd (board !! fst cursor !! snd cursor) /= jogador
        then readMove board cursor jogador window
        else selectMove board cursor jogador window
    KeyChar 'q' -> return (-1, -1, -1, -1)
    _ -> readMove board cursor jogador window

-- Permite que o jogador selecione um movimento válido
selectMove :: [[(Int, Int)]] -> (Int, Int) -> Int -> Window -> IO (Int, Int, Int, Int)
selectMove board piece jogador window = do
  let validMoves = getValidMoves board piece
  if null validMoves
    then readMove board piece jogador window
    else moveSelectionLoop board piece (head validMoves) validMoves jogador window


moveSelectionLoop :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Window -> IO (Int, Int, Int, Int)
moveSelectionLoop board piece cursor validMoves jogador window = do
  boardGenerate cursor board window
  ev <- getCh
  case ev of
    KeyChar 'w' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves jogador window
    KeyChar 's' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves jogador window
    KeyChar 'a' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves jogador window
    KeyChar 'd' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves jogador window
    KeyChar '\n' -> return (fst piece, snd piece, fst cursor, snd cursor)
    KeyChar 'q' -> return (-1, -1, -1, -1)
    _ -> moveSelectionLoop board piece cursor validMoves jogador window

-- Move o cursor apenas dentro das posições válidas
moveCursorList :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
moveCursorList validMoves currentCursor =
  case dropWhile (/= currentCursor) validMoves of
    (_:next:_) -> next
    _ -> head validMoves

-- Verifica se um jogador ainda pode mover alguma peça
isBlocked :: [[(Int, Int)]] -> Int -> Bool
isBlocked board jogador =
  null [(r, c) | r <- [0..7], c <- [0..7], snd (board !! r !! c) == jogador, not (null (getValidMoves board (r, c))) ]