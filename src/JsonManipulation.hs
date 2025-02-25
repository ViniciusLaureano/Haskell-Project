module JsonManipulation (saveFinalGameState, saveGame) where

import GameState (GameState(..), Phase(..))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)

saveFinalGameState :: GameState -> IO()
saveFinalGameState gameState = B.writeFile "json/saveHistory.json" (encode gameState)


saveGame :: GameState -> IO ()
saveGame gameState = B.writeFile "json/saveGame.json" (encode gameState)
