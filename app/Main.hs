import UI.HSCurses.Curses

-- Opções do menu
menuOptions :: [String]
menuOptions = ["Novo Jogo", "Continue", "Histórico", "Sair"]

-- Função principal
main :: IO ()
main = do
  initCurses
  cBreak True
  echo False
  keypad stdScr True
  cursSet CursorInvisible

  (rows, cols) <- scrSize  -- Obtém o tamanho da tela
  loop (rows `div` 2 - 2)  -- Começa com a primeira opção selecionada

  endWin

-- Loop principal do menu
loop :: Int -> IO ()
loop selectedIndex = do
  wclear stdScr
  (rows, cols) <- scrSize

  let title = "Nine Men Morris"
      titlePos = (rows `div` 2 - 4, (cols - length title) `div` 2)

  -- Exibe o título
  mvWAddStr stdScr (fst titlePos) (snd titlePos) title

  -- Exibe as opções do menu
  mapM_ (\(i, option) -> 
          let y = (rows `div` 2 - 2) + i
              x = (cols - length option) `div` 2
              prefix = if i == selectedIndex then "> " else "  "
          in mvWAddStr stdScr y (x - 2) (prefix ++ option)
        ) (zip [0..] menuOptions)

  refresh
  key <- getCh

  -- Lógica de navegação
  case key of
    (KeyChar 'w') -> loop (max 0 (selectedIndex - 1)) -- Sobe no menu
    (KeyChar 's') -> loop (min (length menuOptions - 1) (selectedIndex + 1)) -- Desce no menu
    (KeyChar '\n') -> handleSelection selectedIndex -- Enter para selecionar
    _             -> loop selectedIndex -- Mantém a posição

-- Lógica de seleção
handleSelection :: Int -> IO ()
handleSelection index = case index of
  0 -> mvWAddStr stdScr 0 0 "Novo Jogo selecionado" >> refresh >> getCh >> main
  1 -> mvWAddStr stdScr 0 0 "Continue selecionado" >> refresh >> getCh >> main
  2 -> mvWAddStr stdScr 0 0 "Histórico selecionado" >> refresh >> getCh >> main
  3 -> return ()  -- Sai do programa
  _ -> main
