module Tutorial (startTutorial) where

import UI.HSCurses.Curses

import WindowManipulation

import Board

import GameComponents

import Stage1Functions

matrix = matrixDefault


matrixSecondStep::[[(Int, Int)]]
matrixSecondStep = [[(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)],
  [(-1, -1), (1, 1), (0, 0), (0, 0), (1, 2), (0, 0), (0, 0), (1, 1), (-1, -1)],
  [(-1, -1), (0, 0), (1, 1), (0, 0), (1, 1), (0, 0), (1, 2), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 2), (1, 2), (1, 1), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (1, 2), (1, 2), (1, 2), (-1, -1), (1, 1), (1, 2), (1, 2), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 1), (1, 1), (1, 2), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 2), (0, 0), (1, 1), (0, 0), (1, 1), (0, 0), (-1, -1)],
  [(-1, -1), (1, 1), (0, 0), (0, 0), (1, 2), (0, 0), (0, 0), (1, 2), (-1, -1)],
  [(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)]]

matrixThirdStep::[[(Int, Int)]]
matrixThirdStep = [[(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)],
  [(-1, -1), (1, 0), (0, 0), (0, 0), (1, 2), (0, 0), (0, 0), (1, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (1, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 0), (1, 0), (1, 0), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (1, 2), (1, 0), (1, 2), (-1, -1), (1, 0), (1, 2), (1, 2), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 1), (1, 1), (1, 2), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 2), (0, 0), (1, 2), (0, 0), (1, 2), (0, 0), (-1, -1)],
  [(-1, -1), (1, 0), (0, 0), (0, 0), (1, 0), (0, 0), (0, 0), (1, 1), (-1, -1)],
  [(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)]]

startTutorial :: Window -> IO()
startTutorial window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "O Nine Men's Morris (também conhecido como 'Moinho' ou 'Jogo do Moinho') é um jogo de estratégia" window
  writeScreenCenter 1 "antigo e clássico para dois jogadores. O objetivo do jogo é formar 'moinhos' (linhas de três peças) para capturar" window
  writeScreenCenter 2 "as peças do oponente. O jogo é dividido em três fases: colocação de peças, movimentação de peças e fase final" window
  writeScreenCenter 3 "(quando um jogador tem apenas três peças)." window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  key <- getCh
  case key of
    KeyRight -> firstStep window
    KeyChar 'q' -> return ()
    _ -> startTutorial window

firstStep :: Window -> IO()
firstStep window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Esse será o seu tabuleiro durante o jogo se familiarize com ele :) " window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (2, 4) matrix window
  key <- getCh
  case key of
    KeyRight -> upMove matrix window (2, 4)
    KeyChar 'q' -> return() 
    _ -> startTutorial window
    
upMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
upMove matrix window (y, x) = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "W A S D será a sua movimentação durante o jogo" window
  writeScreenCenter 1 "Teste apertando W pra mover o cursor para cima" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (y, x) matrix window
  key <- getCh
  case key of
    KeyChar 'w' -> upMove matrix window (1, 4)
    KeyRight -> rightMove matrix window (1, 4)
    KeyLeft -> startTutorial window
    KeyChar 'q' -> return ()
    _ -> upMove matrix window (y, x)

rightMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
rightMove matrix window (y, x) = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Aperte D para mover o cursor para a direita" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (y, x) matrix window
  key <- getCh
  case key of
    KeyChar 'd' -> rightMove matrix window (1, 7)
    KeyRight -> leftMove matrix window (1, 7)
    KeyLeft -> upMove matrix window (2, 4)
    KeyChar 'q' -> return ()
    _ -> rightMove matrix window (y, x)

leftMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
leftMove matrix window (y, x) = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Aperte A para mover o cursor para a esquerda" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (y, x) matrix window
  key <- getCh
  case key of
    KeyChar 'a' -> leftMove matrix window (1, 4)
    KeyRight -> downMove matrix window (1, 4)
    KeyLeft -> rightMove matrix window (1, 4)
    KeyChar 'q' -> return ()
    _ -> leftMove matrix window (y, x)
 
downMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
downMove matrix window (y, x) = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Aperte S para mover o cursor para baixo" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (y, x) matrix window
  key <- getCh
  case key of
    KeyChar 's' -> downMove matrix window (2, 4)
    KeyRight -> makeMove matrix window (2, 4)
    KeyLeft -> leftMove matrix window (1, 7)
    KeyChar 'q' -> return ()
    _ -> downMove matrix window (y, x)

makeMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
makeMove matrix window (y, x)= do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Aperte ENTER para colocar uma peça na posição do cursor" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (y, x) matrix window
  key <- getCh
  case key of
    KeyChar '\n' -> do
      (updatedmatrix, _) <- markPosition matrix (2, 4) 1 False window
      makeMove updatedmatrix window (1, 4)
    KeyLeft -> downMove matrixDefault window (1, 4)
    KeyRight -> intermission matrix window 
    KeyChar 'q' -> return ()
    _ -> makeMove matrix window (2, 4)
  
intermission :: [[(Int, Int)]] -> Window -> IO()
intermission matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Nesse jogo, para sair do primeiro estágio é necessário colocar todas as peças no tabuleiro." window
  writeScreenCenter 1 "entrando no segundo estágio,as peças podem começar a se mover para casas adjacentes" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  let updatedmatrix = matrixSecondStep
  boardGenerate (1, 1) updatedmatrix window
  key <- getCh
  case key of
    KeyRight -> removePiece updatedmatrix window
    KeyLeft -> makeMove matrix window (2, 4)
    KeyChar 'q' -> return ()
    _ -> intermission matrix window

removePiece :: [[(Int, Int)]] -> Window -> IO()
removePiece matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "ao formar um moínho(3 peças alinhadas) o jogador pode" window
  writeScreenCenter 1 "selecionar uma peça do seu openente e remove-la, nesse caso o" window
  writeScreenCenter 2 "jogador 2 pode escolher e remover a peça do jogador 1, vamos remover a peça (2, 4)" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (1, 1) matrix window
  let updatedmatrix = removeOpponentPiece matrix (2, 4)
  key <- getCh
  case key of
    KeyRight -> thirdStep updatedmatrix window
    KeyLeft -> intermission matrixDefault window 
    KeyChar 'q' -> return()
    _ -> removePiece matrix window

thirdStep :: [[(Int, Int)]] -> Window -> IO()
thirdStep matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "O jogo seguirá com ambos os jogadores retirando peças do seu openente" window
  writeScreenCenter 1 "Quando algum jogador tiver com apenas 3 peças ele entrará no estágio 3" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate (1, 1) matrix window
  let updatedmatrix = matrixThirdStep
  key <- getCh
  case key of
    KeyRight -> freeMove updatedmatrix window
    KeyLeft -> removePiece matrixSecondStep window
    KeyChar 'q' -> return ()
    _ -> thirdStep matrix window


freeMove :: [[(Int, Int)]] -> Window -> IO()
freeMove matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Como o jogador 1 tem apenas 3 peças no tabuleiro, ele entra no terceiro estágio" window
  writeScreenCenter 1"Sua movimentação agora não está mais limitada apenas às casas adjacentes" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  boardGenerate(1, 1) matrix window
  let updatedmatrix = removeOpponentPiece matrix (5, 4)
  key <- getCh 
  case key of
    KeyRight -> playerTwoWins updatedmatrix window
    KeyLeft -> thirdStep (removeOpponentPiece (matrixSecondStep) (2, 4)) window
    KeyChar 'q' -> return ()
    _ -> freeMove matrix window


playerTwoWins :: [[(Int, Int)]] -> Window -> IO()
playerTwoWins matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Mesmo no terceiro estágio o jogo continua, e quem ficar com 2 peças" window
  writeScreenCenter 1 "restantes primeiro perde. Nesse caso o jogador 2 ganhou o jogo" window
  writeScreen 11 (cols `div` 2 + 22) "➡ para avançar " window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  
  boardGenerate (1, 1) matrix window
  key <- getCh
  case key of
    KeyRight -> lastStep matrix window
    KeyLeft -> freeMove matrixThirdStep window
    KeyChar 'q' -> return ()
    _ -> playerTwoWins matrix window


lastStep :: [[(Int, Int)]] -> Window -> IO()
lastStep matrix window = do
  (rows, cols) <- scrSize
  clearAndWriteScreenCenter 0 "Agora que você conhece as regras e estratégias básicas, é hora de jogar! Divirta-se com o Nine Men's Morris" window
  writeScreenCenter 1 "um jogo que combina estratégia, planejamento e antecipação." window
  writeScreen 12 (cols `div` 2 + 22) "⬅ para voltar " window
  writeScreen 13 (cols `div` 2 + 22) "Digite 'q' para sair do tutorial" window
  key <- getCh
  case key of
    KeyLeft -> playerTwoWins matrix window
    KeyChar 'q' -> return ()
    _ -> lastStep matrix window
    
  
  











