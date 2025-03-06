module HistoryFunctionatlities (loadHistoryJSON, toListFromJSON, showGameHistory, showGames) where

import Data.Aeson (decode)
import UI.HSCurses.Curses

import qualified Data.ByteString.Lazy as B

import GameState (GameState(..))
import GameHistoryList (GameHistoryList(..))

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

showGames :: [GameState] -> Int -> Window -> IO ()
showGames [] _ _ = return ()
showGames (GameState board _ (playerNum, p1, p2) _ _ _ : rest) index window = do
  clearAndWriteScreen 0 0 ("Game #" ++ (show index)) window
  writeScreen 1 0 ("Players: " ++ p1 ++ " vs " ++ p2) window
  writeScreen 2 0 ("Winner: " ++ winner) window
  boardHistory board window
  writeScreen 4 0 "Press Enter to see the next game..." window
  _ <- getCh
  showGames rest (index + 1) window

  where
  winner = if playerNum == 1 then p1 else p2


