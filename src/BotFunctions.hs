module BotFunctions (findPlace, randomChoice) where

import System.Random
import GameComponents

matrix = matrixDefault
findPlace :: [[(Int, Int)]] -> IO (Int, Int)
findPlace matrix = do
  i <- randomRIO (1, 7)
  j <- randomRIO (1, 7)
  let tuple = matrix !! i !! j :: (Int, Int)
  if fst tuple == 1 && snd tuple == 0 then return (i, j)
  else findPlace matrix

randomChoice :: [a] -> IO a
randomChoice xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)