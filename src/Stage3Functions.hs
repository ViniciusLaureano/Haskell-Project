module Stage3Functions (readPiece3, movePieceStageThree, botMove) where

import UI.HSCurses.Curses

import Board
import WindowManipulation
import BotFunctions
import Stage1Functions
import Stage2Functions


isCellOccupied :: [[(Int, Int)]] -> (Int, Int) -> Bool
isCellOccupied board (r, c) = (board !! r) !! c /= (1, 0)


isPlayerPiece :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool
isPlayerPiece board (r, c) player = case (board !! r) !! c of
  (1, p) -> p == player
  _     -> False


movePieceStageThree :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> Int -> Bool -> Window -> IO ([[(Int, Int)]], Bool)
movePieceStageThree board (r1, c1) (r2, c2) player bot window = do
  if not (isPlayerPiece board (r1, c1) player)
    then do
      return (board, False)
    else do
      if isCellOccupied board (r2, c2)
        then do
          return (board, False)
        else do
          let boardWithoutPiece = take r1 board ++ [take c1 (board !! r1) ++ [(1, 0)] ++ drop (c1 + 1) (board !! r1)] ++ drop (r1 + 1) board
          let boardWithPiece = take r2 boardWithoutPiece ++ [take c2 (boardWithoutPiece !! r2) ++ [(1, player)] ++ drop (c2 + 1) (boardWithoutPiece !! r2)] ++ drop (r2 + 1) boardWithoutPiece

          if isMillFormed boardWithPiece (r2, c2) player
            then do
              (updatedBoard, mill) <- handleMillRemoval boardWithPiece player bot window
              return (updatedBoard, mill)
            else return (boardWithPiece, False)


readPiece3 :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool -> Window -> IO (Int, Int)
readPiece3 board cursor player bot window = do
  boardGenerate cursor board window
  if bot && player == 2
    then do
    botCursor <- findPlace board
    return botCursor
  else do
    ev <- getCh
    case ev of
        KeyChar 'w' -> readPiece3 board (moveCursor board cursor (-1, 0)) player bot window
        KeyChar 's' -> readPiece3 board (moveCursor board cursor (1, 0)) player bot window
        KeyChar 'a' -> readPiece3 board (moveCursor board cursor (0, -1)) player bot window
        KeyChar 'd' -> readPiece3 board (moveCursor board cursor (0, 1)) player bot window
        KeyChar '\n' -> return cursor
        KeyChar 'q' -> return (-1,-1)
        _ -> readPiece3 board cursor player bot window

botMove :: [[(Int, Int)]] -> Int -> Window -> IO ([[ (Int, Int) ]], Bool)
botMove board player window = do
  let validMoves = [(r1, c1, r2, c2) | r1 <- [0..8], c1 <- [0..8], 
                                        let startPos = (r1, c1),
                                        isPlayerPiece board startPos player,
                                        r2 <- [0..8], c2 <- [0..8],
                                        not (isCellOccupied board (r2, c2))]
  
  if null validMoves
    then return (board, False)
    else do
      move <- randomChoice validMoves
      let (r1, c1, r2, c2) = move
      movePieceStageThree board (r1, c1) (r2, c2) player True window


