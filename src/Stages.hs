module Stages (stage1, stage2, stage3) where

import UI.HSCurses.Curses

import WindowManipulation
import Validations
import Stage1Functions
import Stage2Functions

stage1 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage1 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinho bot window = 
  if validateStage1 totJogadas 
    then stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinho bot window
  else do
    mostraJogador jogador nomeJogador1 nomeJogador2 window

    cursor <- readPiece matriz (1, 1) jogador totJogadas moinho bot window

    if cursor == (-1, -1) then
      clearAndWriteScreen 0 0 "Save game" window
      -- save game de alexandre
    else do
      (novaMatriz, moinhoAtual) <- markPosition matriz cursor jogador window
      if novaMatriz /= matriz 
        then stage1 novaMatriz (totJogadas + 1) ((jogador `mod` 2 + 1), nomeJogador1, nomeJogador2) moinhoAtual bot window
      else
        stage1 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinhoAtual bot window

stage2 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ()
stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinho bot window = 
  if validateStage2 matriz 
    then stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinho bot window
  else do
    clearAndWriteScreen 0 0 "Stage 2" window

    (r1, c1, r2, c2) <- readMove matriz (1, 1) jogador window

    if (r1, c1, r2, c2) == (-1, -1, -1, -1) then
      clearAndWriteScreen 0 0 "Save game" window

    else do    
      let newBoard = movePiece matriz (r1, c1) (r2, c2) jogador
      let moinhoFormado = isMillFormed newBoard (r2, c2) jogador

      (finalBoard, moinhoAtivo) <- if moinhoFormado

        then handleMillRemoval newBoard jogador window
        else return (newBoard, False)

      stage2 finalBoard (totJogadas + 1) ((jogador `mod` 2 + 1), nomeJogador1, nomeJogador2) moinhoAtivo bot window


stage3 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool ->Bool -> Window -> IO ()
stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) moinho bot window = 
  if validateStage3 matriz 
    then finishGame matriz totJogadas (jogador, nomeJogador1, nomeJogador2)
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 