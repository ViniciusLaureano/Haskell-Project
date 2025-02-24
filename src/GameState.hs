{-# LANGUAGE DeriveGeneric #-}

module GameState where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

data GameState = GameState
  { board :: [[(Int, Int)]]
  , rounds :: Int
  , players :: (Int, String, String)
  , phase :: Phase
  , window :: Window
  } deriving (Show, Generic)

data Phase = Phase1 | Phase2 | Phase3 deriving (Show, Eq, Generic)

instance TojSON GameState
instance FromJSON GameState