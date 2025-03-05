module Stages (stage1, stage2, stage3) where

import UI.HSCurses.Curses

import WindowManipulation
import Validations
import Stage1Functions
import Stage2Functions
import Stage3Functions
import GameState
import JsonManipulation

stage1 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage1 matriz totRounds (player, nickname1, nickname2) moinho bot window = 
  if validateStage1 totRounds 
    then stage2 matriz totRounds (player, nickname1, nickname2) moinho bot window
  else do
    mostraJogador player nickname1 nickname2 window

    cursor <- readPiece matriz (1, 1) player totRounds moinho bot window

    if cursor == (-1, -1) then do
      let state = GameState { 
        gameBoard = matriz
      , rounds = totRounds
      , players = (player, nickname1, nickname2)
      , phase = Phase1
      , bot = True
      }
      saveToBeContinuedGame state
      clearAndWriteScreen 0 0 "Save game" window
    else do
      (novaMatriz, moinhoAtual) <- markPosition matriz cursor player bot window
      if novaMatriz /= matriz 
        then stage1 novaMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) moinhoAtual bot window
      else
        stage1 matriz totRounds (player, nickname1, nickname2) moinhoAtual bot window

stage2 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage2 matriz totRounds (player, nickname1, nickname2) moinho bot window = 
  if validateStage2 matriz 
    then stage3 matriz totRounds (player, nickname1, nickname2) moinho bot window
  else do
    clearAndWriteScreen 0 0 "Stage 2" window
    
    (r1, c1, r2, c2) <- readMove matriz (1, 1) player bot window

    if (r1, c1, r2, c2) == (-1, -1, -1, -1) then
      clearAndWriteScreen 0 0 "Save game" window

    else do    
      let newBoard = movePiece matriz (r1, c1) (r2, c2) player
      let moinhoFormado = isMillFormed newBoard (r2, c2) player

      (finalBoard, moinhoAtivo) <- if moinhoFormado

        then handleMillRemoval newBoard player bot window
        else return (newBoard, False)

      stage2 finalBoard (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) moinhoAtivo bot window


stage3 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage3 matriz totRounds (player, nickname1, nickname2) moinho bot window = 
  if validateStage3 matriz 
    then finishGame matriz totRounds (player, nickname1, nickname2)
  else do
    mostraJogador player nickname1 nickname2 window

    if bot && player == 2
      then do
      (novaMatriz, _) <- botMove matriz player window
      stage3 novaMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) moinho bot window
    else do
      (r1, c1) <- readPiece3 matriz (1, 1) player bot window
      if (r1, c1) == (-1, -1)
        then clearAndWriteScreen 0 0 "Save game" window
        else do
        (r2, c2) <- readPiece3 matriz (r1, c1) player bot window
        (novaMatriz, moinhoAtual) <- movePieceStageThree matriz (r1, c1) (r2, c2) player bot window
        if novaMatriz /= matriz 
          then stage3 novaMatriz (totRounds + 1) ((player `mod` 2 + 1), nickname1, nickname2) moinhoAtual bot window
        else
          stage3 novaMatriz totRounds (player, nickname1, nickname2) moinhoAtual bot window