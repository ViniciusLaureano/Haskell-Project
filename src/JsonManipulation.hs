module JsonManipulation (saveFinalGameState, saveGame) where

import GameState (GameState(..), Phase(..))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import Data.Maybe (fromMaybe)

saveFinalGameState :: GameState -> IO()
saveFinalGameState gameState = do
    -- Read the existing file (if it exists)
    existingData <- B.readFile "json/saveHistory.json"

    -- Decode existing JSON array or default to an empty list
    let existingGames :: [GameState]
        existingGames = fromMaybe [] (decode existingData)

    -- Append the new game state
    let updatedGames = existingGames ++ [gameState]

    -- Write the updated array back to the file
    B.writeFile "json/saveHistory.json" (encode updatedGames)


saveGame :: GameState -> IO ()
saveGame gameState = B.writeFile "json/saveGame.json" (encode gameState)
