module MenuFunctionalities (newGame, continueGame, matchHistory) where

import UI.HSCurses.Curses

import WindowManipulation

import Stages

--import Data.Char (chr)

import Control.Concurrent (threadDelay)

import GameComponents

newGame :: Window -> IO ()
newGame window = do

  wclear window 
  (rows, cols) <- scrSize
  let linhaCentral = rows `div` 2

  centralizarMensagem (linhaCentral - 2) "Bem-vindo ao Nine Men's Morris!" window
  wRefresh window

  threadDelay 2000000

  centralizarMensagem (linhaCentral) "Digite o nome do Jogador 1: " window

  let colunaCentral = (cols - length "Digite o nome do Jogador 1: ") `div` 2
  nomeJogador1 <- getString (linhaCentral + 1) colunaCentral window

  wRefresh window

  centralizarMensagem (linhaCentral + 3) "Digite o nome do Jogador 2: " window
  nomeJogador2 <- getString (linhaCentral + 4) colunaCentral window

  wRefresh window

  stage1 matrizDefault 0 (1, nomeJogador1, nomeJogador2) window

continueGame :: Window -> IO ()
continueGame window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

matchHistory :: Window -> IO ()
matchHistory window = do
  clearAndWriteScreen 0 0 "Not implemented yet" window

getString :: Int -> Int -> Window -> IO String
getString linha coluna window = loop ""
  where
    loop str = do
      -- Move o cursor para a posição correta e exibe a string atual
      mvWAddStr window linha coluna (str ++ replicate (50 - length str) ' ')  -- Limpa a área de digitação
      mvWAddStr window linha coluna str  -- Exibe a string atual
      wRefresh window

      -- Captura um caractere
      ch <- getCh  -- getCh retorna um valor do tipo Key
      case keyToChar ch of
        -- Enter (código 10 ou 13)
        Just '\n' -> return str  -- Retorna a string quando o usuário pressiona Enter
        Just '\r' -> return str  -- Alternativa para Enter
        -- Backspace (código 127 ou 8)
        Just '\DEL' -> handleBackspace str
        Just '\BS' -> handleBackspace str
        -- Outros caracteres
        Just c -> do
          -- Adiciona o caractere à string
          let newStr = str ++ [c]
          loop newStr
        -- Tecla não reconhecida (ignora)
        Nothing -> loop str

    -- Função para tratar o Backspace
    handleBackspace str
      | null str = loop str  -- Se a string estiver vazia, não faz nada
      | otherwise = do
          -- Remove o último caractere da string
          let newStr = init str
          loop newStr

    -- Função para converter Key em Char
    keyToChar :: Key -> Maybe Char
    keyToChar (KeyChar c) = Just c  -- Caracteres comuns
    keyToChar key = case key of
      KeyEnter -> Just '\n'  -- Enter
      KeyBackspace -> Just '\DEL'  -- Backspace
      _ -> Nothing  -- Outras teclas especiais (ignora)


centralizarMensagem :: Int -> String -> Window -> IO ()
centralizarMensagem linha msg window = do
  (_, cols) <- scrSize
  let x = (cols - length msg) `div` 2
  mvWAddStr window linha x msg
  wRefresh window