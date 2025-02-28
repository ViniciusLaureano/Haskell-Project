module Stages (stage1, stage2, stage3) where

import UI.HSCurses.Curses

import WindowManipulation
import Validations
import Stage1Functions
import GameState
import JsonManipulation

stage1 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Window -> IO ()
stage1 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) isBot window = 
  if validateStage1 totJogadas 
    then stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) isBot window
  else do
    mostraJogador jogador nomeJogador1 nomeJogador2 window

    cursor <- readPiece matriz (1, 1) window

    if cursor == (-1, -1) then do
      let state = GameState { 
        gameBoard = matriz
      , rounds = totJogadas
      , players = (jogador, nomeJogador1, nomeJogador2)
      , phase = Phase1
      , isBot = isBot
      }
      saveGame state
      clearAndWriteScreen 0 0 "Save game" window
    else do
      let novaMatriz = markPosition matriz cursor jogador
      stage1 novaMatriz (totJogadas + 1) ((jogador `mod` 2 + 1), nomeJogador1, nomeJogador2) isBot window


stage2 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Window -> IO ()
stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) isBot window = 
  if validateStage2 matriz 
    then stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) isBot window
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 


stage3 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Window -> IO ()
stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) isBot window = 
  if validateStage3 matriz 
    then finishGame matriz totJogadas (jogador, nomeJogador1, nomeJogador2)
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 