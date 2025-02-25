module MenuFunctionalities (newGame, continueGame, saveGame, tutorial, matchHistory) where

import UI.HSCurses.Curses
import Data.Aeson (encode, decode)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B

import Board 
import Stages
import GameComponents
import WindowManipulation
import GameState (GameState(..), Phase(..))

newGame :: Window -> IO ()
newGame window = do
  (rows, cols) <- scrSize
  let centerRow = rows `div` 2

  clearAndWriteScreenCenter (centerRow - 2) "Bem-vindo ao Nine Men's Morris!" window

  threadDelay 1500000

  writeScreenCenter centerRow "Digite o nome do Jogador 1: " window

  let colunaCentral = (cols - length "Digite o nome do Jogador 1: ") `div` 2
  nomeJogador1 <- getString (centerRow + 1) colunaCentral window

  writeScreenCenter (centerRow + 3) "Digite o nome do Jogador 2: " window
  nomeJogador2 <- getString (centerRow + 4) colunaCentral window


  stage1 matrizDefault 0 (1, nomeJogador1, nomeJogador2) window


continueGame :: Window -> IO ()
continueGame window = do
  jsonData <- B.readFile "json/saveGame.json"
  let loadedGame = decode jsonData :: Maybe GameState

  case loadedGame of 
    Just state -> do
      let phaseFunc = gamePhase state
      phaseFunc (gameBoard state) (rounds state) (players state) window
    Nothing -> clearAndWriteScreen 0 0 "Erro ao carregar jogo" window


tutorial :: Window -> IO ()
tutorial window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window


matchHistory :: Window -> IO ()
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window


gamePhase :: GameState -> ([[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ())
gamePhase state
  | phase state == Phase1 = stage1
  | phase state == Phase2 = stage2
  | phase state == Phase3 = stage3
  | otherwise = error "Invalid game phase"


getString :: Int -> Int -> Window -> IO String
getString linha coluna window = loop ""
  where
    loop str = do
      mvWAddStr window linha coluna (str ++ replicate (50 - length str) ' ')
      mvWAddStr window linha coluna str

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


saveFinalGameState :: GameState -> IO()
saveFinalGameState gameState = B.writeFile "json/saveHistory.json" (encode gameState)


saveGame :: GameState -> IO ()
saveGame gameState = B.writeFile "json/saveGame.json" (encode gameState)
