module MenuFunctionalities (newGame, continueGame, tutorial, matchHistory) where

import UI.HSCurses.Curses
import Data.Aeson (encode, decode)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B

import Stages
import Tutorial
import GameComponents
import HistoryFunctionatlities
import WindowManipulation
import GameState (GameState(..), Phase(..))

newGame :: Window -> IO ()
newGame window = do
  (rows, cols) <- scrSize
  let centerRow = rows `div` 2

  clearAndWriteScreenCenter (centerRow - 2) "Bem-vindo ao Nine Men's Morris!" window

  threadDelay 500000

  isBot <- againstBot centerRow window

  clearAndWriteScreenCenter centerRow "Digite o nome do Jogador 1: " window

  let centerColumn = (cols - length "Digite o nome do Jogador 1: ") `div` 2
  nickname1 <- getString (centerRow + 1) centerColumn window

  if isBot then
    writeScreenCenter (centerRow + 3) "Digite o nome do Bot:" window
  else
    writeScreenCenter (centerRow + 3) "Digite o nome do Jogador 2: " window

  nickname2 <- getString (centerRow + 4) centerColumn window

  stage1 matrixDefault 1 (1, nickname1, nickname2) False isBot window

  where
    againstBot :: Int -> Window -> IO Bool
    againstBot centerRow window = do
        writeScreenCenter centerRow "Digite 1 para o modo PvP ou 2 para PvE" window
        ch <- getCh
        verifyOption centerRow ch window

    verifyOption :: Int -> Key -> Window -> IO Bool
    verifyOption _ (KeyChar '1') _ = return False
    verifyOption _ (KeyChar '2') _ = return True
    verifyOption centerRow _ window = againstBot centerRow window

continueGame :: Window -> IO ()
continueGame window = do
  jsonData <- B.readFile "json/saveGame.json"
  let loadedGame = decode jsonData :: Maybe GameState

  case loadedGame of 
    Just state -> do
      let phaseFunc = gamePhase state
      phaseFunc (gameBoard state) (rounds state) (players state) (mill state) (bot state) window
    Nothing -> clearAndWriteScreen 0 0 "Erro ao carregar jogo" window


tutorial :: Window -> IO ()
tutorial window = do
  startTutorial window


matchHistory :: Window -> IO ()
matchHistory window = do
  jsonData <- loadHistoryJSON
  let historyList = toListFromJSON jsonData
  showGameHistory historyList window

  -- a função showGameHistory em HistoryFunctionatlities.hs é um exemplo de como iterar por essa lista pra mostrar na tela
  clearAndWriteScreen 0 0 "Not implemented yet" window


getString :: Int -> Int -> Window -> IO String
getString row column window = loop ""
  where
    loop str = do
      writeScreen row column (str ++ replicate (50 - length str) ' ') window
      writeScreen row column str window
      ch <- getCh
      case keyToChar ch of
        Just '\n' -> return str
        Just '\r' -> return str
        Just '\DEL' -> handleBackspace str
        Just '\BS' -> handleBackspace str

        Just c -> do
          let newStr = str ++ [c]
          loop newStr
        Nothing -> loop str


    handleBackspace str
      | null str = loop str
      | otherwise = do
          let newStr = init str
          loop newStr

    keyToChar :: Key -> Maybe Char
    keyToChar (KeyChar c) = Just c
    keyToChar key = case key of
      KeyEnter -> Just '\n'
      KeyBackspace -> Just '\DEL'
      _ -> Nothing

gamePhase :: GameState -> ([[(Int, Int)]] -> Int -> (Int, String, String) -> Bool -> Bool -> Window -> IO ())
gamePhase state
  | phase state == Phase1 = stage1
  | phase state == Phase2 = stage2
  | phase state == Phase3 = stage3
  | otherwise = error "Invalid game phase"