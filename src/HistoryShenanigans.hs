module HistoryShenanigans where

import Data.Aeson (encode, decode)
import UI.HSCurses.Curses

import qualified Data.ByteString.Lazy as B

import GameState (GameState(..), Phase(..))
import GameHistoryList (GameHistoryList(..))

import Data.Maybe (fromMaybe)

import WindowManipulation
import Board

loadHistoryJSON :: IO B.ByteString
loadHistoryJSON = B.readFile "json/saveHistory.json"

toListFromJSON :: B.ByteString -> [GameState]
toListFromJSON jsonData = case decode jsonData of
          Just (GameHistoryList games) -> games
          Nothing -> []

showGameHistory :: [GameState] -> Window -> IO ()
showGameHistory [] window = clearAndWriteScreenCenter 0 "No game history available." window
showGameHistory states window = showGames states 1 window

showGames :: [GameState] -> Window -> IO ()
showGames [] _ window = return ()
showGames (GameState board _ (playerNum, p1, p2) _ _ _ : rest) index window = do
  clearAndWriteScreen 0 ("Game #" ++ (show index)) window
  writeScreen 0 0 ("Players: " ++ p1 ++ " vs " ++ p2) window
  writeScreen 0 1 ("Winner: " ++ winner) window
  writeScreen 0 3 ("Board:") window
  boardGenerate (1, 1) board window
  writeScreen 0 4 "Press Enter to see the next game..." window
  _ <- getLine  -- Waits for user input
  showGames rest (index + 1) window -- Recursively show next game

  where
  winner = if playerNum == 1 then p1 else p2


