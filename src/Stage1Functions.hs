module Stage1Functions (readPiece, moveCursor, isValidPosition, isMillFormed, selectOpponentPiece, selectOpponentPieceFromList, removeOpponentPiece, markPosition, showPlayer) where

import UI.HSCurses.Curses
import System.Random (randomRIO)

import Board
import BotFunctions
import WindowManipulation


isValidPosition :: [[(Int, Int)]] -> (Int, Int) -> Bool
isValidPosition matriz (r, c) =
  let (v, _) = matriz !! r !! c
  in v == 1


isOutOfBounds :: [[(Int, Int)]] -> (Int, Int) -> Bool
isOutOfBounds matriz (r, c) =
  r < 0 || r >= length matriz || c < 0 || c >= length (head matriz) || matriz !! r !! c == (-1, -1)


findValidPosition :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findValidPosition matriz (r, c) (dr, dc)
  | isOutOfBounds matriz (r, c) = (r - dr, c - dc)
  | isValidPosition matriz (r, c) = (r, c)
  | otherwise = findValidPosition matriz (r + dr, c + dc) (dr, dc)


moveCursor :: [[(Int, Int)]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveCursor matriz (r, c) (dr, dc) =
  let newPos = findValidPosition matriz (r + dr, c + dc) (dr, dc)
  in if isValidPosition matriz newPos then newPos else (r, c)


markPosition :: [[(Int, Int)]] -> (Int, Int) -> Int -> Bool -> Window -> IO ([[ (Int, Int) ]], Bool)
markPosition board (r, c) player bot window = do
  if board !! r !! c /= (1, 0) && board !! r !! c  /= (1, player) 
    then return (board, False)
    else do
      let newBoard = take r board ++ [take c (board !! r) ++ [(1, player)] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board
      let millAtual = isMillFormed newBoard (r, c) player  
      let millAntigo = isMillFormed board (r, c) player  

      if millAtual && not millAntigo  
        then do
          let oponente = if player == 1 then 2 else 1
          let todasPecasOponente = [(r', c') | r' <- [0..7], c' <- [0..7], snd (board !! r' !! c') == oponente]
          let pecasNaoMoinho = filter (\pos -> not (isMillFormed board pos oponente)) todasPecasOponente

          posToRemove <- if bot && player == 2
            then do
              if null pecasNaoMoinho  
                then return (head todasPecasOponente)
                else return (head pecasNaoMoinho)
            else if null pecasNaoMoinho  
                then selectOpponentPiece newBoard oponente window
                else selectOpponentPieceFromList newBoard oponente pecasNaoMoinho window

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
readPiece matriz cursor player totRounds millIsFormed bot window = do
  boardGenerate cursor matriz window
  if bot && player == 2
    then do
      botCursor <- findPlace matriz
      return botCursor
  else do
    ev <- getCh
    case ev of
      KeyChar 'w' -> readPiece matriz (moveCursor matriz cursor (-1, 0)) player totRounds millIsFormed bot window
      KeyChar 's' -> readPiece matriz (moveCursor matriz cursor (1, 0)) player totRounds millIsFormed bot window
      KeyChar 'a' -> readPiece matriz (moveCursor matriz cursor (0, -1)) player totRounds millIsFormed bot window
      KeyChar 'd' -> readPiece matriz (moveCursor matriz cursor (0, 1)) player totRounds millIsFormed bot window
      KeyChar '\n' -> return cursor
      KeyChar 'q' -> return (-1, -1)
      _ -> readPiece matriz cursor player totRounds millIsFormed bot window


showPlayer :: Int -> String -> String -> Window -> IO ()
showPlayer player nickname1 nickname2 window = 
  if player == 1 then 
    clearAndWriteScreenCenter 1 nickname1 window
  else
    clearAndWriteScreenCenter 1 nickname2 window