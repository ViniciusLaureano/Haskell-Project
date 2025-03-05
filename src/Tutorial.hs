module Tutorial (startTutorial) where

import UI.HSCurses.Curses

import WindowManipulation

import Board

import GameComponents

import Stage1Functions

matriz = matrizDefault


matrizSecondStep::[[(Int, Int)]]
matrizSecondStep = [[(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)],
  [(-1, -1), (1, 1), (0, 0), (0, 0), (1, 2), (0, 0), (0, 0), (1, 1), (-1, -1)],
  [(-1, -1), (0, 0), (1, 1), (0, 0), (1, 1), (0, 0), (1, 2), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 2), (1, 2), (1, 1), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (1, 2), (1, 2), (1, 2), (-1, -1), (1, 1), (1, 2), (1, 2), (-1, -1)],
  [(-1, -1), (0, 0), (0, 0), (1, 1), (1, 1), (1, 2), (0, 0), (0, 0), (-1, -1)],
  [(-1, -1), (0, 0), (1, 2), (0, 0), (1, 1), (0, 0), (1, 1), (0, 0), (-1, -1)],
  [(-1, -1), (1, 1), (0, 0), (0, 0), (1, 2), (0, 0), (0, 0), (1, 2), (-1, -1)],
  [(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)]]

matrizThirdStep::[[(Int, Int)]]
matrizThirdStep = [[(-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1), (-1, -1)],
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
  clearAndWriteScreen 3 30 "O Nine Men's Morris (também conhecido como 'Moinho' ou 'Jogo do Moinho') é um jogo de estratégia" window
  writeScreen 4 30 "antigo e clássico para dois jogadores. O objetivo do jogo é formar 'moinhos' (linhas de três peças) para capturar" window
  writeScreen 5 30 "as peças do oponente. O jogo é dividido em três fases: colocação de peças, movimentação de peças e fase final" window
  writeScreen 6 30 "(quando um jogador tem apenas três peças)." window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 13 100 "q para sair do tutorial" window
  key <- getCh
  case key of
    KeyRight -> firstStep window
    KeyChar 'q' -> return ()
    _ -> startTutorial window

firstStep :: Window -> IO()
firstStep window = do
  clearAndWriteScreen 4 60 "Esse será o seu tabuleiro durante o jogo se familiarize com ele :) " window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (2, 4) matriz window
  key <- getCh
  case key of
    KeyRight -> upMove matriz window (2, 4)
    KeyChar 'q' -> return() 
    _ -> startTutorial window
    
upMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
upMove matriz window (y, x) = do
  clearAndWriteScreen 4 60 "W A S D será a sua movimentação durante o jogo" window
  writeScreen 10 60 "Teste apertando W pra mover o cursor para cima" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'w' -> upMove matriz window (1, 4)
    KeyRight -> rightMove matriz window (1, 4)
    KeyLeft -> startTutorial window
    KeyChar 'q' -> return ()
    _ -> upMove matriz window (y, x)

rightMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
rightMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte D para mover o cursor para a direita" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'd' -> rightMove matriz window (1, 7)
    KeyRight -> leftMove matriz window (1, 7)
    KeyLeft -> upMove matriz window (2, 4)
    KeyChar 'q' -> return ()
    _ -> rightMove matriz window (y, x)

leftMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
leftMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte A para mover o cursor para a esquerda" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 'a' -> leftMove matriz window (1, 4)
    KeyRight -> downMove matriz window (1, 4)
    KeyLeft -> rightMove matriz window (1, 4)
    KeyChar 'q' -> return ()
    _ -> leftMove matriz window (y, x)
 
downMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
downMove matriz window (y, x) = do
  clearAndWriteScreen 10 60 "Aperte S para mover o cursor para baixo" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar 's' -> downMove matriz window (2, 4)
    KeyRight -> makeMove matriz window (2, 4)
    KeyLeft -> leftMove matriz window (1, 7)
    KeyChar 'q' -> return ()
    _ -> downMove matriz window (y, x)

makeMove :: [[(Int, Int)]] -> Window -> (Int, Int) -> IO()
makeMove matriz window (y, x)= do
  clearAndWriteScreen 10 60 "Aperte ENTER para colocar uma peça na posição do cursor" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (y, x) matriz window
  key <- getCh
  case key of
    KeyChar '\n' -> do
      (updatedMatriz, _) <- markPosition matriz (2, 4) 1 False window
      makeMove updatedMatriz window (1, 4)
    KeyLeft -> downMove matrizDefault window (1, 4)
    KeyRight -> intermission matriz window 
    KeyChar 'q' -> return ()
    _ -> makeMove matriz window (2, 4)
  
intermission :: [[(Int, Int)]] -> Window -> IO()
intermission matriz window = do
  clearAndWriteScreen 9 40 "Nesse jogo, para sair do primeiro estágio é necessário colocar todas as peças no tabuleiro." window
  writeScreen 10 40 "entrando no segundo estágio,as peças podem começar a se mover para casas adjacentes" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  let updatedMatriz = matrizSecondStep
  boardGenerate (1, 1) updatedMatriz window
  key <- getCh
  case key of
    KeyRight -> removePiece updatedMatriz window
    KeyLeft -> makeMove matriz window (2, 4)
    KeyChar 'q' -> return ()
    _ -> intermission matriz window

removePiece :: [[(Int, Int)]] -> Window -> IO()
removePiece matriz window = do
  clearAndWriteScreen 8 60 "ao formar um moínho(3 peças alinhadas) o jogador pode" window
  writeScreen 9 60 "selecionar uma peça do seu openente e remove-la, nesse caso o" window
  writeScreen 10 60 "jogador 2 pode escolher e remover a peça do jogador 1, vamos remover a peça (2, 4)" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (1, 1) matriz window
  let updatedMatriz = removeOpponentPiece matriz (2, 4)
  key <- getCh
  case key of
    KeyRight -> thirdStep updatedMatriz window
    KeyLeft -> intermission matrizDefault window 
    KeyChar 'q' -> return()
    _ -> removePiece matriz window

thirdStep :: [[(Int, Int)]] -> Window -> IO()
thirdStep matriz window = do
  clearAndWriteScreen 9 60 "O jogo seguirá com ambos os jogadores retirando peças do seu openente" window
  writeScreen 10 60 "Quando algum jogador tiver com apenas 3 peças ele entrará no estágio 3" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate (1, 1) matriz window
  let updatedMatriz = matrizThirdStep
  key <- getCh
  case key of
    KeyRight -> freeMove updatedMatriz window
    KeyLeft -> removePiece matrizSecondStep window
    KeyChar 'q' -> return ()
    _ -> thirdStep matriz window


freeMove :: [[(Int, Int)]] -> Window -> IO()
freeMove matriz window = do
  clearAndWriteScreen 9 60 "Como o jogador 1 tem apenas 3 peças no tabuleiro, ele entra no terceiro estágio" window
  writeScreen 10 60 "Sua movimentação agora não está mais limitada apenas às casas adjacentes" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  boardGenerate(1, 1) matriz window
  let updatedMatriz = removeOpponentPiece matriz (5, 4)
  key <- getCh 
  case key of
    KeyRight -> playerTwoWins updatedMatriz window
    KeyLeft -> thirdStep (removeOpponentPiece (matrizSecondStep) (2, 4)) window
    KeyChar 'q' -> return ()
    _ -> freeMove matriz window


playerTwoWins :: [[(Int, Int)]] -> Window -> IO()
playerTwoWins matriz window = do
  clearAndWriteScreen 9 60 "Mesmo no terceiro estágio o jogo continua, e quem ficar com 2 peças" window
  writeScreen 10 60 "restantes primeiro perde. Nesse caso o jogador 2 ganhou o jogo" window
  writeScreen 11 100 "➡ para avançar " window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  
  boardGenerate (1, 1) matriz window
  key <- getCh
  case key of
    KeyRight -> lastStep matriz window
    KeyLeft -> freeMove matrizThirdStep window
    KeyChar 'q' -> return ()
    _ -> playerTwoWins matriz window


lastStep :: [[(Int, Int)]] -> Window -> IO()
lastStep matriz window = do
  clearAndWriteScreen 9 40 "Agora que você conhece as regras e estratégias básicas, é hora de jogar! Divirta-se com o Nine Men's Morris" window
  writeScreen 10 40 "um jogo que combina estratégia, planejamento e antecipação." window
  writeScreen 12 100 "⬅ para voltar " window
  writeScreen 13 100 "q para sair do tutorial" window
  key <- getCh
  case key of
    KeyLeft -> playerTwoWins matriz window
    KeyChar 'q' -> return ()
    _ -> lastStep matriz window
    
  
  











