module Stage1Functions (readPiece, moveCursor, isValidPosition, isMillFormed, selectOpponentPiece, selectOpponentPieceFromList, removeOpponentPiece, markPosition, showPlayer) where

import UI.HSCurses.Curses
import System.Random (randomRIO)

import Board
import BotFunctions
import WindowManipulation


isValidPosition :: [[(Int, Int)]] -> (Int, Int) -> Bool
isValidPosition matrix (r, c) =
  let (v, _) = matrix !! r !! c
  in v == 1


isOutOfBounds :: [[(Int, Int)]] -> (Int, Int) -> Bool
isOutOfBounds matrix (r, c) =
  r < 0 || r >= length matrix || c < 0 || c >= length (head matrix) || matrix !! r !! c == (-1, -1)


findValidPosition :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findValidPosition matrix (r, c) (dr, dc)
  | isOutOfBounds matrix (r, c) = (r - dr, c - dc)
  | isValidPosition matrix (r, c) = (r, c)
  | otherwise = findValidPosition matrix (r + dr, c + dc) (dr, dc)


moveCursor :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveCursor matrix (r, c) (dr, dc) =
  let newPos = findValidPosition matrix (r + dr, c + dc) (dr, dc)
  in if isValidPosition matrix newPos then newPos else (r, c)


markPosition :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool -> Window -> IO ([[ (Int, Int) ]], Bool)
markPosition board (r, c) player bot window = do
  if board !! r !! c /= (1, 0) && board !! r !! c  /= (1, player) 
    then return (board, False)
    else do
      let newBoard = take r board ++ [take c (board !! r) ++ [(1, player)] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board
      let millCurrent = isMillFormed newBoard (r, c) player  
      let millOld = isMillFormed board (r, c) player  

      if millCurrent && not millOld  
        then do
          let oponent = if player == 1 then 2 else 1
          let allOpponentPieces = [(r', c') | r' <- [0..7], c' <- [0..7], snd (board !! r' !! c') == oponent]
          let piecesNotInMill = filter (\pos -> not (isMillFormed board pos oponent)) allOpponentPieces

          posToRemove <- if bot && player == 2
            then do
              if null piecesNotInMill  
                then return (head allOpponentPieces)
                else return (head piecesNotInMill)
            else if null piecesNotInMill  
                then selectOpponentPiece newBoard oponent window
                else selectOpponentPieceFromList newBoard oponent piecesNotInMill window

          if posToRemove == (-1, -1)
            then return (newBoard, True)  
            else do
              let updatedBoard = removeOpponentPiece newBoard posToRemove
              return (updatedBoard, True)  
        else return (newBoard, False)


isMillFormed :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool
isMillFormed board (r, c) player =
  let checkMill [(x1, y1), (x2, y2), (x3, y3)] =
        all (\(x, y) -> snd (board !! x !! y) == player) [(x1, y1), (x2, y2), (x3, y3)]
      mills = [
          -- Linhas horizontais
          [(1,1), (1,4), (1,7)], [(2,2), (2,4), (2,6)], [(3,3), (3,4), (3,5)],
          [(5,3), (5,4), (5,5)], [(6,2), (6,4), (6,6)], [(7,1), (7,4), (7,7)],
          -- Linhas verticais
          [(1,1), (4,1), (7,1)], [(2,2), (4,2), (6,2)], [(3,3), (4,3), (5,3)],
          [(3,5), (4,5), (5,5)], [(2,6), (4,6), (6,6)], [(1,7), (4,7), (7,7)],
          -- Linhas centrais
          [(4,1), (4,2), (4,3)], [(1,4), (2,4), (3,4)], [(4,5), (4,6), (4,7)], [(5,4), (6,4), (7,4)]
        ]
  in any (\mill -> (r, c) `elem` mill && checkMill mill) mills


removeOpponentPiece :: [[(Int, Int)]] -> (Int, Int) -> [[(Int, Int)]]
removeOpponentPiece board (r, c) =
  let (tipo, marker) = board !! r !! c
  in if marker /= 0
     then take r board 
          ++ [take c (board !! r) ++ [(tipo, 0)] ++ drop (c + 1) (board !! r)]
          ++ drop (r + 1) board
     else board


selectOpponentPiece :: [[(Int, Int)]] -> Int -> Window -> IO (Int, Int)
selectOpponentPiece board opponent window = do
  let opponentPieces = [(r, c) | r <- [0..8], c <- [0..8], isValidPosition board (r, c), snd (board !! r !! c) == opponent]
  if null opponentPieces
    then return (-1, -1)
    else do
      let (initialR, initialC) = head opponentPieces
      removeLoop board opponent (initialR, initialC) window


selectOpponentPieceFromList :: [[(Int, Int)]] -> Int -> [(Int, Int)] -> Window -> IO (Int, Int)
selectOpponentPieceFromList board player allowedPositions window = do
  if null allowedPositions
    then selectOpponentPiece board player window
    else do
      posToRemove <- selectOpponentPiece board player window  
      if posToRemove `elem` allowedPositions
        then return posToRemove
        else do
          selectOpponentPieceFromList board player allowedPositions window


removeLoop :: [[(Int, Int)]] -> Int -> (Int, Int) -> Window -> IO (Int, Int)
removeLoop board opponent cursor window = do
  boardGenerate cursor board window
  ev <- getCh
  case ev of
    KeyChar 'w' -> removeLoop board opponent (moveCursor board cursor (-1, 0)) window
    KeyChar 's' -> removeLoop board opponent (moveCursor board cursor (1, 0)) window
    KeyChar 'a' -> removeLoop board opponent (moveCursor board cursor (0, -1)) window
    KeyChar 'd' -> removeLoop board opponent (moveCursor board cursor (0, 1)) window
    KeyChar '\n' -> if snd (board !! fst cursor !! snd cursor) == opponent
                   then return cursor
                   else removeLoop board opponent cursor window
    _ -> removeLoop board opponent cursor window


readPiece :: [[(Int, Int)]] -> (Int, Int) -> Int -> Int -> Bool -> Bool -> Window -> IO (Int, Int)
readPiece matrix cursor player totRounds millIsFormed bot window = do
  boardGenerate cursor matrix window
  if bot && player == 2
    then do
      botCursor <- findPlace matrix
      return botCursor
  else do
    ev <- getCh
    case ev of
      KeyChar 'w' -> readPiece matrix (moveCursor matrix cursor (-1, 0)) player totRounds millIsFormed bot window
      KeyChar 's' -> readPiece matrix (moveCursor matrix cursor (1, 0)) player totRounds millIsFormed bot window
      KeyChar 'a' -> readPiece matrix (moveCursor matrix cursor (0, -1)) player totRounds millIsFormed bot window
      KeyChar 'd' -> readPiece matrix (moveCursor matrix cursor (0, 1)) player totRounds millIsFormed bot window
      KeyChar '\n' -> return cursor
      KeyChar 'q' -> return (-1, -1)
      _ -> readPiece matrix cursor player totRounds millIsFormed bot window


showPlayer :: Int -> String -> String -> Window -> IO ()
showPlayer player nickname1 nickname2 window = 
  if player == 1 then 
    clearAndWriteScreenCenter 1 nickname1 window
  else
    clearAndWriteScreenCenter 1 nickname2 window