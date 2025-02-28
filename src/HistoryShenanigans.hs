module HistoryShenanigans where

import Data.Aeson (encode, decode)

import qualified Data.ByteString.Lazy as B

import GameState (GameState(..), Phase(..))

import Data.Maybe (fromMaybe)

loadHistoryJSON :: IO B.ByteString
loadHistoryJSON = B.readFile "json/saveHistory.json"

toListFromJSON :: B.ByteString -> [GameState]
toListFromJSON jsonData = fromMaybe [] (decode jsonData)

showGameHistory :: [GameState] -> IO ()
showGameHistory [] = putStrLn "No game history available."
showGameHistory states = showGames states 1
  where
    showGames [] _ = return ()
    showGames (GameState board _ (playerNum, p1, p2) _ _ : rest) index = do
      let winner = if playerNum == 1 then p1 else p2
      putStrLn $ "Game #" ++ show index
      putStrLn $ "Players: " ++ p1 ++ " vs " ++ p2
      putStrLn $ "Winner: " ++ winner
      putStrLn "Board:"
      print board  -- Displays the board
      putStrLn "\nPress Enter to see the next game..."
      _ <- getLine  -- Waits for user input
      showGames rest (index + 1)  -- Recursively show next game

