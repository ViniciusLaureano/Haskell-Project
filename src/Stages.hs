module Stages (stage1, stage2, stage3) where

import UI.HSCurses.Curses

import WindowManipulation
import Validations

stage1 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ()
stage1 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) window = 
  if validateStage1 totJogadas 
    then stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) window
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 


stage2 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ()
stage2 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) window = 
  if validateStage2 matriz 
    then stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) window
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 


stage3 :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ()
stage3 matriz totJogadas (jogador, nomeJogador1, nomeJogador2) window = 
  if validateStage3 matriz 
    then finishGame matriz totJogadas (jogador, nomeJogador1, nomeJogador2)
  else do
    clearAndWriteScreen 0 0 "Not implemented yet" window 