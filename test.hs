{-# LANGUAGE ScopedTypeVariables #-}

import UI.HSCurses.Curses
import Control.Monad
import Control.Exception

loop :: IO () = do
    refresh
    c <- getCh
    when (c /= KeyChar 'q') $ do
        wAddStr stdScr (show c)
        addLn
        loop

main :: IO ()
main = do
    initCurses
    cBreak True
    echo False
    keypad stdScr True
    loop `finally` endWin
