import UI.HSCurses.Curses


matriz :: [[(Int, Int)]]
matriz = replicate 7 (replicate 7 (-1, -1))


criaListaDePontos ::[[(Int, Int)]] -> [[(Int, Int)]]
criaListaDePontos matriz = [[(y, x) | (y, x) <- linha, x /= -1, y /= -1] | linha <- matriz]


atualizaMatriz :: [[(t,t)]] -> (Int, Int) -> (t,t) -> [[(t,t)]]
atualizaMatriz matriz (linha,coluna) e = 
    let (antes, alvo : depois) = splitAt linha matriz
        (antesC, alvoC : depoisC) = splitAt coluna alvo
    in  antes ++ [antesC ++ [e] ++ depoisC] ++ depois


-- Função para desenhar o tabuleiro
desenhaTabuleiro :: [[(Int, Int)]] -> IO ()
desenhaTabuleiro matriz = do
  mvWAddStr stdScr 14 60 "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓"
  mvWAddStr stdScr 30 60 "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"
  desenhaVertical 60
  desenhaVertical 90
  desenhaPontos matriz
  refresh
  

desenhaPontos :: [[(Int, Int)]] -> IO ()
desenhaPontos matriz = do
  let listaPontos = criaListaDePontos matriz
  sequence_ [mvWAddStr stdScr y x "*" | linha <- listaPontos, (y, x) <- linha]

desenhaVertical :: Int -> IO()
desenhaVertical x = sequence_ [mvWAddStr stdScr y x "┃" | y <- [15..29]]


fazerJogada :: Int -> Int -> [[(Int, Int)]] -> [[(Int, Int)]]
fazerJogada y x matriz = atualizaMatriz matriz ((mod y 7), (mod x 7)) (y, x)



-- Função principal
main :: IO ()
main = do
  initCurses                  -- Inicializa o modo ncurses
  cBreak True                 -- Habilita o modo cBreak (teclas são lidas imediatamente)
  echo False                  -- Desabilita a exibição de caracteres digitados
  keypad stdScr True          -- Habilita o uso de teclas especiais
  cursSet CursorInvisible     -- Oculta o cursor

  loop (14, 60) matriz                -- Inicia o loop com a posição inicial do caractere

  endWin                      -- Finaliza o modo ncurses

-- Loop principal do programa
loop :: (Int, Int) -> [[(Int, Int)]]-> IO ()
loop (y, x) matriz = do
  wclear stdScr               -- Limpa a janela padrão (stdScr)
  desenhaTabuleiro matriz           -- Desenha o tabuleiro
  mvWAddStr stdScr y x "X"    -- Desenha o caractere na posição (y, x)
  refresh                     -- Atualiza a tela

  key <- getCh                -- Lê uma tecla do usuário

  -- Atualiza a posição com base na tecla pressionada
  case key of
    (KeyChar 'w') -> loop (max 14 (y - 8), x) matriz      -- Move para cima
    (KeyChar 's') -> loop (min 30 (y + 8), x) matriz -- Move para baixo
    (KeyChar 'a') -> loop (y, max 60 (x - 15)) matriz       -- Move para a esquerda
    (KeyChar 'd') -> loop (y, min 90 (x + 15)) matriz -- Move para a direita
    (KeyChar 'q') -> return ()                     -- Sai do loop
    (KeyChar '\n') -> do
      let novaMatriz = fazerJogada y x matriz
      loop (y, x) novaMatriz
    _             -> loop (y, x) matriz                   -- Tecla inválida, mantém a posição


  

