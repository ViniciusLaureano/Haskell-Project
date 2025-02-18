import UI.HSCurses.Curses

import Menu


main :: IO ()
main = do
  initCurses
  cBreak True
  echo False
  keypad stdScr True
  cursSet CursorInvisible
  
  mainMenu 0 stdScr