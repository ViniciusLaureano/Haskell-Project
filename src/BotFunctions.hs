module BotFunctions (findPlace) where

import System.Random
import GameComponents

matriz = matrizDefault

findPlace :: [[(Int, Int)]] -> IO (Int, Int)
findPlace matriz = do
  i <- randomRIO (1, 7)
  j <- randomRIO (1, 7)
  let tupla = matriz !! i !! j :: (Int, Int)
  if fst tupla == 1 && snd tupla == 0 then return (i, j)
  else findPlace matriz

