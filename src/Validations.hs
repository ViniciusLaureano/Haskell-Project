module Validations (validateStage1, validateStage2, validateStage3, finishGame) where

import JsonManipulation
import GameState (GameState(..), Phase(..))


validateStage1 :: Int -> Bool
validateStage1 totJogadas = totJogadas == 19


validateStage2 :: [[(Int, Int)]] -> Bool
validateStage2 matriz = playerPieces matriz 1 == 3 || playerPieces matriz 2 == 3


validateStage3 :: [[(Int, Int)]] -> Bool
validateStage3 matriz = playerPieces matriz 1 == 2 || playerPieces matriz 2 == 2


finishGame :: [[(Int, Int)]] -> Int -> (Int, String, String) -> IO ()
finishGame matriz totJogadas (jogador, nomeJogador1, nomeJogador2) = do
  let winner = if playerPieces matriz 1 > playerPieces matriz 2 then 1 else 2
  let finalState = GameState { gameBoard = matriz
                             , rounds = totJogadas
                             , players = (winner, nomeJogador1, nomeJogador2)
                             , phase = Phase3
                             , isBot = False }
  saveGameInHistory finalState


playerPieces :: [[(Int, Int)]] -> Int -> Int
playerPieces matriz playerNumber = length [(x, y) | row <- matriz, (x, y) <- row, y == playerNumber]


saveGameInHistory :: GameState -> IO ()
saveGameInHistory gameState = saveFinalGameState gameState
