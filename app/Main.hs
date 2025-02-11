import UI.HSCurses.Curses

-- Função principal
main :: IO ()
main = do
  initCurses                  -- Inicializa o modo ncurses
  cBreak True                 -- Habilita o modo cBreak (teclas são lidas imediatamente)
  echo False                  -- Desabilita a exibição de caracteres digitados
  keypad stdScr True          -- Habilita o uso de teclas especiais (como setas)
  cursSet CursorInvisible     -- Oculta o cursor

  loop (10, 10)               -- Inicia o loop com a posição inicial do caractere

  endWin                      -- Finaliza o modo ncurses

-- Loop principal do programa
loop :: (Int, Int) -> IO ()
loop (y, x) = do
  wclear stdScr               -- Limpa a janela padrão (stdScr)
  mvWAddStr stdScr y x "@"    -- Desenha o caractere na posição (y, x)
  refresh                     -- Atualiza a tela

  key <- getCh                -- Lê uma tecla do usuário

  -- Atualiza a posição com base na tecla pressionada
  case key of
    (KeyChar 'w') -> loop (y - 1, x)    -- Move para cima
    (KeyChar 's') -> loop (y + 1, x)    -- Move para baixo
    (KeyChar 'a') -> loop (y, x - 1)    -- Move para a esquerda
    (KeyChar 'd') -> loop (y, x + 1)    -- Move para a direita
    (KeyChar 'q') -> return ()          -- Sai do loop
    _             -> loop (y, x)        -- Tecla inválida, mantém a posição