module JsonManipulation (saveFinalGameState, saveToBeContinuedGame) where

import GameState (GameState(..), Phase(..))
import GameHistoryList (GameHistoryList(..))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import Data.Maybe (fromMaybe)

import UI.HSCurses.Curses
import WindowManipulation

saveFinalGameState :: GameState -> Window -> IO ()
saveFinalGameState gameState window = do
    -- Read the existing file (if it exists)
  existingData <- B.readFile "json/saveHistory.json"
    

  -- Decodificar o JSON ou criar uma lista vazia caso falhe
  let existingGames = case decode existingData of
          Just (GameHistoryList games) -> games
          Nothing -> []

  -- Atualizar lista com o novo estado do jogo
  let updatedGames = GameHistoryList (existingGames ++ [gameState])
  -- clearAndWriteScreenCenter 0 (show (updatedGames)) window
  clearAndWriteScreenCenter 0 "Partida Finalisada!" window
  writeScreenCenter 1 "(aperte qualquer tecla para continuar)" window
  ch <- getCh

  -- Salvar no arquivo
  B.writeFile "json/saveHistory.json" (encode updatedGames)
      

saveToBeContinuedGame :: GameState -> IO ()
saveToBeContinuedGame gameState = B.writeFile "json/saveGame.json" (encode gameState)
