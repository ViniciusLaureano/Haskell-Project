module GameComponents (matrixDefault) where

matrixDefault :: [[(Int, Int)]]
matrixDefault = [[(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)],
  [(-1, -1), (1, 0), (0, 0), (0, 0), (1, 0), (0, 0), (0, 0), (1, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 0), (1, 0), (1, 0), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (1, 0), (1, 0), (1, 0), (-1, -1), (1, 0), (1, 0), (1, 0), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 0), (1, 0), (1, 0), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (-1, -1)],
  [(-1, -1), (1, 0), (0, 0), (0, 0), (1, 0), (0, 0), (0, 0), (1, 0), (-1, -1)],
  [(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)]]