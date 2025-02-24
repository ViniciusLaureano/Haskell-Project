module Menu (mainMenu) where

import UI.HSCurses.Curses

import WindowManipulation
import MenuFunctionalities

mainMenu :: Int -> Window -> IO ()
mainMenu selectedIndex window = do
  (rows, cols) <- scrSize
  wclear window 

  let titlePosition = (rows `div` 2 - 4, (cols - length title) `div` 2)

  clearAndWriteScreen (fst titlePosition) (snd titlePosition) title window

  mapM_ (\(i, option) -> 
          let y = (rows `div` 2 - 2) + i
              x = (cols - length option) `div` 2 - 2
              prefix = if i == selectedIndex then "> " else "  "
          in writeScreen y x (prefix ++ option) window
        ) options

  key <- getCh

  case key of
    (KeyChar 'w') -> mainMenu (max 0 (selectedIndex - 1)) window
    (KeyChar 's') -> mainMenu (min (length menuOptions - 1) (selectedIndex + 1)) window
    (KeyChar '\n') -> handleSelection selectedIndex window
    _ -> mainMenu selectedIndex window
    
    

  where
    title = "Nine Men Morris"
    options = (zip [0..] menuOptions)


menuOptions :: [String]
menuOptions = ["Novo Jogo", "Continue", "Tutorial", "Histórico", "Sair"]


handleSelection :: Int -> Window -> IO ()
handleSelection index window
  | index == 0 = newGame window >> mainMenu 0 window
  | index == 1 = continueGame window >> mainMenu 0 window
  | index == 2 = tutorial window >> mainMenu 0 window
  | index == 3 = matchHistory window >> mainMenu 0 window
  | index == 4 = endWin
  | otherwise = mainMenu 0 window