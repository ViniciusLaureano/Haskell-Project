module Validations (validateStage1, validateStage2, validateStage3, finishGame, playerPieces) where

import JsonManipulation
import GameState (GameState(..), Phase(..))
import UI.HSCurses.Curses


validateStage1 :: Int -> Bool
validateStage1 totRounds = totRounds == 19


validateStage2 :: [[(Int, Int)]] -> Int -> Bool
validateStage2 matriz jogador = playerPieces matriz jogador <= 3


validateStage3 :: [[(Int, Int)]] -> Bool
validateStage3 matriz = playerPieces matriz 1 == 2 || playerPieces matriz 2 == 2


finishGame :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Window -> IO ()
finishGame matriz totRounds (jogador, nickname1, nickname2) bot window = do
  let winner = if playerPieces matriz 1 > playerPieces matriz 2 then 1 else 2
  saveFinalGameState GameState { 
                              gameBoard = matriz
                              , rounds = totRounds
                              , players = (winner, nickname1, nickname2)
                              , phase = Phase3
                              , mill = True
                              , bot = bot } window


playerPieces :: [[(Int, Int)]] -> Int -> Int
playerPieces matriz playerNumber = length [(x, y) | row <- matriz, (x, y) <- row, y == playerNumber]

