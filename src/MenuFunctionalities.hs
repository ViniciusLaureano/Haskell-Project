module MenuFunctionalities (newGame, continueGame, saveGame, tutorial, matchHistory) where

import UI.HSCurses.Curses

import WindowManipulation

import Data.Aeson (encode, decode)

import qualified Data.ByteString.Lazy as B

import GameState (GameState(..), Phase(..))

import Stages

import Control.Concurrent (threadDelay)

import GameComponents

newGame :: Window -> IO ()
newGame window = do

  wclear window 
  (rows, cols) <- scrSize
  let linhaCentral = rows `div` 2

  centralizarMensagem (linhaCentral - 2) "Bem-vindo ao Nine Men's Morris!" window
  wRefresh window

  threadDelay 2000000

  centralizarMensagem (linhaCentral) "Digite o nome do Jogador 1: " window

  let colunaCentral = (cols - length "Digite o nome do Jogador 1: ") `div` 2
  nomeJogador1 <- getString (linhaCentral + 1) colunaCentral window

  wRefresh window

  centralizarMensagem (linhaCentral + 3) "Digite o nome do Jogador 2: " window
  nomeJogador2 <- getString (linhaCentral + 4) colunaCentral window

  wRefresh window

  stage1 matrizDefault 0 (1, nomeJogador1, nomeJogador2) window

continueGame :: Window -> IO ()
continueGame window = do
  jsonData <- B.readFile "json/saveGame.json"
  let loadedGame = decode jsonData :: Maybe GameState

  case loadedGame of 
    Just state -> do
      let phaseFunc = gamePhase state
      phaseFunc (gameBoard state) (rounds state) (players state) window
    Nothing -> putStrLn "Erro ao carregar jogo"


tutorial :: Window -> IO ()
tutorial window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window


matchHistory :: Window -> [([[(Int, Int)]], (Int, String, String))]
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window
