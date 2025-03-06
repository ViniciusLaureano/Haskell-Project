module JsonManipulation (saveFinalGameState, saveToBeContinuedGame) where

import GameState (GameState(..), Phase(..))
import GameHistoryList (GameHistoryList(..))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import Data.Maybe (fromMaybe)
import System.IO (withFile, openFile, hClose, IOMode(ReadMode), IOMode(WriteMode))

import UI.HSCurses.Curses
import WindowManipulation

saveFinalGameState :: GameState -> Window -> IO ()
saveFinalGameState gameState window = do
    -- Read the existing file (if it exists)
  existingData <- B.readFile "json/saveHistory.json"

  --handle <- openFile "json/saveHistory.json" ReadMode
  --existingData <- B.hGetContents handle
  --hClose handle  -- Ensure the file is closed
  
  -- Decode the JSON or create an empty list if that fails
  let existingGames = case decode existingData of
          Just (GameHistoryList games) -> games
          Nothing -> []

  -- Update the list with the new game state
  let updatedGames = GameHistoryList (existingGames ++ [gameState])
  -- clearAndWriteScreenCenter 0 (show (updatedGames)) window
  clearAndWriteScreenCenter 0 "Partida Finalizada!" window
  writeScreenCenter 1 "(aperte qualquer tecla para continuar)" window
  --ch <- getCh

  fileSaver updatedGames
  -- Save in the file
  --handle <- openFile "json/saveHistory.json" WriteMode
  --B.hPut writeHandle (encode updatedGames)
  --hClose writeHandle 
  --B.writeFile "json/saveHistory.json" (encode updatedGames)

fileSaver :: GameHistoryList -> IO()
fileSaver updatedGames = do
  -- Use 'withFile' to ensure the handle is properly managed
  withFile "json/saveHistory.json" WriteMode $ \handle -> do
    B.hPut handle (encode updatedGames)
      

saveToBeContinuedGame :: GameState -> IO ()
saveToBeContinuedGame gameState = B.writeFile "json/saveGame.json" (encode gameState)
