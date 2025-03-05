module Stages (stage1, stage2, stage3) where

import UI.HSCurses.Curses

import GameState
import Validations
import Stage1Functions
import Stage2Functions
import Stage3Functions
import JsonManipulation
import WindowManipulation

stage1 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage1 matriz totRounds (player, nickname1, nickname2) mill bot window = 
  if validateStage1 totRounds 
    then stage2 matriz totRounds (player, nickname1, nickname2) mill bot window
  else do
    showPlayer player nickname1 nickname2 window
    writeScreenCenter 0 "Fase 1" window
    writeScreen 0 0 ("Rodada: " ++ (show totRounds)) window

    cursor <- readPiece matriz (1, 1) player totRounds mill bot window

    if cursor == (-1, -1) then
      saveToBeContinuedGame GameState { 
        gameBoard = matriz
        , rounds = totRounds
        , players = (player, nickname1, nickname2)
        , phase = Phase1
        , mill = mill
        , bot = bot
      }
    else do
      (newMatriz, currentMill) <- markPosition matriz cursor player bot window
      if newMatriz /= matriz 
        then stage1 newMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) currentMill bot window
      else
        stage1 matriz totRounds (player, nickname1, nickname2) currentMill bot window


stage2 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage2 matriz totRounds (player, nickname1, nickname2) mill bot window = 
  if validateStage2 matriz player
    then stage3 matriz totRounds (player, nickname1, nickname2) mill bot window
  else do
    showPlayer player nickname1 nickname2 window
    writeScreenCenter 0 "Fase 2" window
    writeScreen 0 0 ("Rodada: " ++ (show totRounds)) window

    (r1, c1, r2, c2) <- readMove matriz (1, 1) player bot window

    if (r1, c1, r2, c2) == (-1, -1, -1, -1) then
      saveToBeContinuedGame GameState { 
        gameBoard = matriz
        , rounds = totRounds
        , players = (player, nickname1, nickname2)
        , phase = Phase1
        , mill = mill
        , bot = bot
      }
    else do    
      let newBoard = movePiece matriz (r1, c1) (r2, c2) player
      let millFormado = isMillFormed newBoard (r2, c2) player

      (finalBoard, millAtivo) <- if millFormado

        then handleMillRemoval newBoard player bot window
        else return (newBoard, False)

      stage2 finalBoard (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) millAtivo bot window


stage3 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage3 matriz totRounds (player, nickname1, nickname2) mill bot window = 
  if playerPieces matriz player > 3
    then stage2 matriz totRounds (player, nickname1, nickname2) mill bot window
    else do
      if validateStage3 matriz 
        then finishGame matriz totRounds (player, nickname1, nickname2) bot window
      else do
        showPlayer player nickname1 nickname2 window
        writeScreenCenter 0 "Fase 3" window
        writeScreen 0 0 ("Rodada: " ++ (show totRounds)) window

        if bot && player == 2
          then do
          (newMatriz, _) <- botMove matriz player window
          stage3 newMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) mill bot window
        else do
          (r1, c1) <- readPiece3 matriz (1, 1) player bot window
          if (r1, c1) == (-1, -1) then 
            saveToBeContinuedGame GameState { 
              gameBoard = matriz
              , rounds = totRounds
              , players = (player, nickname1, nickname2)
              , phase = Phase1
              , mill = mill
              , bot = bot
            }
            else do
            (r2, c2) <- readPiece3 matriz (r1, c1) player bot window
            (newMatriz, currentMill) <- movePieceStageThree matriz (r1, c1) (r2, c2) player bot window
            if newMatriz /= matriz 
              then stage3 newMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) currentMill bot window
            else
              stage3 newMatriz totRounds (player, nickname1, nickname2) currentMill bot window