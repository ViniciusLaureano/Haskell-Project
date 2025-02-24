module MenuFunctionalities (newGame, continueGame, saveGame, tutorial, matchHistory) where

import UI.HSCurses.Curses

import WindowManipulation

import Data.Aeson (encode, decode)

import qualified Data.ByteString.Lazy as B

import GameState (GameState(..), Phase(..))

import Stages

newGame :: Window -> IO ()
newGame window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

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


matchHistory :: Window -> IO ()
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window



gamePhase :: GameState -> ([[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ())
gamePhase state
  | phase state == Phase1 = stage1
  | phase state == Phase2 = stage2
  | phase state == Phase3 = stage3
  | otherwise = error "Invalid game phase"

saveGame :: GameState -> IO ()
saveGame gameState = B.writeFile "json/saveGame.json" (encode gameState)