module Lib
    ( main_menu
    ) where

main_menu :: IO ()
main_menu = do
  putStrLn "Bem Vindo ao jogo Nine Men Morris! Escolha uma opção (apenas números)"
  putStrLn "1. Novo Jogo"
  putStrLn "2. Continuar"
  putStrLn "3. Histórico"
  putStrLn "4. Sair"

  input <- readLn :: IO Int

  options input

options :: Int -> IO()
options 1 = putStrLn "opt 1"
options 2 = putStrLn "opt 2"
options 3 = putStrLn "opt 3"
options 4 = putStrLn "opt 4"
options _ = main_menu
