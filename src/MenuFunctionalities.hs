module MenuFunctionalities (newGame, continueGame, saveGame, matchHistory) where

import UI.HSCurses.Curses

import WindowManipulation

import Data.Aeson (FromJSON, ToJSON, encode, decode)

import qualified Data.ByteString.Lazy as B

import GameState (GameState)

import Stages

newGame :: Window -> IO ()
newGame window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

continueGame :: Window -> IO ()
continueGame window = do
  jsonData <- B.readFile "savegame.json"
  let loadedGame = decode jsonData :: Maybe GameState

  case loadedGame of 
    Just state -> do
      let phaseFunc = gamePhase state
      phaseFunc (board state) (rounds state) (players state) window
    Nothing -> putStrLn "Erro ao carregar jogo"

gamePhase :: GameState -> ([[(Int, Int)]] -> Int -> (Int, String, String) -> Window -> IO ())
gamePhase state
  | phase state == Phase1 = stage1
  | phase state == Phase2 = stage2
  | phase state == Phase3 = stage3
  | otherwise = error "Invalid game phase"

saveGame :: GameState -> IO ()
saveGame gameState = B.writeFile "savegame.json" (encode gameState)


matchHistory :: Window -> IO ()
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window
