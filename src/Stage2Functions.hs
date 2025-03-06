module Stage2Functions (readMove, movePiece, handleMillRemoval) where

import UI.HSCurses.Curses

import Board
import WindowManipulation

import Stage1Functions
import BotFunctions


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


movePiece :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> Int -> [[(Int, Int)]]
movePiece board (r1, c1) (r2, c2) player =
  let piecelessBoard = take r1 board ++ [take c1 (board !! r1) ++ [(1, 0)] ++ drop (c1 + 1) (board !! r1)] ++ drop (r1 + 1) board
      boardComPeca = take r2 piecelessBoard ++ [take c2 (piecelessBoard !! r2) ++ [(1, player)] ++ drop (c2 + 1) (piecelessBoard !! r2)] ++ drop (r2 + 1) piecelessBoard
  in boardComPeca


handleMillRemoval :: [[(Int, Int)]] -> Int -> Bool -> Window -> IO ([[ (Int, Int) ]], Bool)
handleMillRemoval board player bot window = do
  let opponent = if player == 1 then 2 else 1
  let allOpponentPieces = [(r', c') | r' <- [0..7], c' <- [0..7], snd (board !! r' !! c') == opponent]
  let piecesNotInMill = filter (\pos -> not (isMillFormed board pos opponent)) allOpponentPieces

  posToRemove <-if bot && player == 2
                  then botRemovePiece board opponent
                else 
                  if null piecesNotInMill
                    then selectOpponentPiece board opponent window
                  else selectOpponentPieceFromList board opponent piecesNotInMill window

  if posToRemove == (-1, -1)
    then return (board, True)  
    else do
      let updatedBoard = removeOpponentPiece board posToRemove
      return (updatedBoard, True)  


readMove :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool -> Window -> IO (Int, Int, Int, Int)
readMove board cursor player bot window = do
  boardGenerate cursor board window
  if bot && player == 2
    then do
      (r1, c1, r2, c2) <- botMove board player
      return (r1, c1, r2, c2)
  else do
    ev <- getCh
    case ev of
      KeyChar 'w' -> readMove board (moveCursor board cursor (-1, 0)) player bot window
      KeyChar 's' -> readMove board (moveCursor board cursor (1, 0)) player bot window
      KeyChar 'a' -> readMove board (moveCursor board cursor (0, -1)) player bot window
      KeyChar 'd' -> readMove board (moveCursor board cursor (0, 1)) player bot window
      KeyChar '\n' -> do
        if snd (board !! fst cursor !! snd cursor) /= player
          then readMove board cursor player bot window
          else selectMove board cursor player bot window
      KeyChar 'q' -> return (-1, -1, -1, -1)
      _ -> readMove board cursor player bot window


selectMove :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool -> Window -> IO (Int, Int, Int, Int)
selectMove board piece player bot window = do
  let validMoves = getValidMoves board piece
  if null validMoves
    then readMove board piece player bot window
    else moveSelectionLoop board piece (head validMoves) validMoves player window


moveSelectionLoop :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Window -> IO (Int, Int, Int, Int)
moveSelectionLoop board piece cursor validMoves player window = do
  boardGenerate cursor board window
  ev <- getCh
  case ev of
    KeyChar 'w' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves player window
    KeyChar 's' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves player window
    KeyChar 'a' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves player window
    KeyChar 'd' -> moveSelectionLoop board piece (moveCursorList validMoves cursor) validMoves player window
    KeyChar '\n' -> return (fst piece, snd piece, fst cursor, snd cursor)
    KeyChar 'q' -> return (-1, -1, -1, -1)
    _ -> moveSelectionLoop board piece cursor validMoves player window


moveCursorList :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
moveCursorList validMoves currentCursor =
  case dropWhile (/= currentCursor) validMoves of
    (_:next:_) -> next
    _ -> head validMoves


isBlocked :: [[(Int, Int)]] -> Int -> Bool
isBlocked board player =
  null [(r, c) | r <- [0..7], c <- [0..7], snd (board !! r !! c) == player, not (null (getValidMoves board (r, c))) ]


botMove :: [[(Int, Int)]] -> Int -> IO (Int, Int, Int, Int)
botMove board player = do
  let pieces = [(r, c) | r <- [0..7], c <- [0..7], snd (board !! r !! c) == player]
  if null pieces
    then return (-1, -1, -1, -1)
    else do
      piece <- randomChoice pieces
      let validMoves = getValidMoves board piece
      if null validMoves
        then botMove board player
        else do
          move <- randomChoice validMoves
          return (fst piece, snd piece, fst move, snd move)


botRemovePiece :: [[(Int, Int)]] -> Int -> IO (Int, Int)
botRemovePiece board player = do
  let opponent = if player == 1 then 2 else 1
  let allOpponentPieces = [(r, c) | r <- [0..7], c <- [0..7], snd (board !! r !! c) == opponent]
  let piecesNotInMill = filter (\pos -> not (isMillFormed board pos opponent)) allOpponentPieces
  if null allOpponentPieces
    then return (-1, -1)
    else if null piecesNotInMill
      then randomChoice allOpponentPieces
      else randomChoice piecesNotInMill
