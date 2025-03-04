{-# LANGUAGE DeriveGeneric #-}

module GameState (GameState(..), Phase(..)) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

data GameState = GameState
  { gameBoard :: [[(Int, Int)]]
  , rounds :: Int
  , players :: (Int, String, String)
  , phase :: Phase
  , moinho :: Bool
  , bot :: Bool
  } deriving (Show, Generic)

data Phase = Phase1 | Phase2 | Phase3 deriving (Show, Eq, Generic)

instance ToJSON Phase
instance FromJSON Phase
instance ToJSON GameState
instance FromJSON GameState