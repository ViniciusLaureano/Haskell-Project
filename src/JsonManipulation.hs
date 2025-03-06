module JsonManipulation (saveFinalGameState, saveToBeContinuedGame) where

import GameState (GameState(..), Phase(..))
import GameHistoryList (GameHistoryList(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, decode)
import Data.Maybe (fromMaybe)
import System.IO (withFile, openFile, hClose, IOMode(ReadMode), IOMode(WriteMode))

import UI.HSCurses.Curses
import WindowManipulation

saveFinalGameState :: GameState -> Window -> IO ()
saveFinalGameState gameState window = do
    -- Read the existing file (if it exists)
  existingData <- BS.readFile "json/saveHistory.json"
  
  -- Decode the JSON or create an empty list if that fails
  let existingGames = case decode (BL.fromStrict existingData) of
          Just (GameHistoryList games) -> games
          Nothing -> []

  -- Update the list with the new game state
  let updatedGames = GameHistoryList (existingGames ++ [gameState])
  -- clearAndWriteScreenCenter 0 (show (updatedGames)) window
  clearAndWriteScreenCenter 2 "Partida Finalizada!" window
  writeScreenCenter 3 "(aperte qualquer tecla para continuar)" window
  ch <- getCh

  fileSaver updatedGames

fileSaver :: GameHistoryList -> IO()
fileSaver updatedGames = do
  -- Use 'withFile' to ensure the handle is properly managed
  withFile "json/saveHistory.json" WriteMode $ \handle -> do
    BL.hPut handle (encode updatedGames)
      

saveToBeContinuedGame :: GameState -> IO ()
saveToBeContinuedGame gameState = BL.writeFile "json/saveGame.json" (encode gameState)
