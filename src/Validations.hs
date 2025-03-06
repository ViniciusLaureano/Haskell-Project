module Validations (validateStage1, validateStage2, validateStage3, finishGame, playerPieces) where

import JsonManipulation
import GameState (GameState(..), Phase(..))
import UI.HSCurses.Curses


validateStage1 :: Int -> Bool
validateStage1 totRounds = totRounds >= 19


validateStage2 :: [[(Int, Int)]] -> Int -> Bool
validateStage2 matrix jogador = playerPieces matrix jogador <= 3


validateStage3 :: [[(Int, Int)]] -> Bool
validateStage3 matrix = playerPieces matrix 1 == 2 || playerPieces matrix 2 == 2


finishGame :: [[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Window -> IO ()
finishGame matrix totRounds (jogador, nickname1, nickname2) bot window = do
  let winner = if playerPieces matrix 1 > playerPieces matrix 2 then 1 else 2
  saveFinalGameState GameState { 
                              gameBoard = matrix
                              , rounds = totRounds
                              , players = (winner, nickname1, nickname2)
                              , phase = Phase3
                              , mill = True
                              , bot = bot } window


playerPieces :: [[(Int, Int)]] -> Int -> Int
playerPieces matrix playerNumber = length [(x, y) | row <- matrix, (x, y) <- row, y == playerNumber]

