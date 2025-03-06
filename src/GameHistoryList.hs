{-# LANGUAGE DeriveGeneric #-}

module GameHistoryList (GameHistoryList(..)) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

import GameState

data GameHistoryList = GameHistoryList
  { gameList :: [GameState]
  } deriving (Show, Generic)

instance ToJSON GameHistoryList
instance FromJSON GameHistoryList