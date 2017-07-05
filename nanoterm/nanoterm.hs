import Graphics.UI.Gtk hiding (beep)
import Graphics.UI.Gtk.Vte.Vte

main :: IO ()
main = do
  initGUI
  window <- windowNew
  onDestroy window mainQuit

  vte <- terminalNew
  containerAdd window vte
  setup vte

  on vte childExited mainQuit
  on vte beep (windowSetUrgencyHint window True)
{-
  on vte windowTitleChanged $
    windowSetTitle window =<< terminalGetWindowTitle vte
-}

  widgetShowAll window
  mainGUI


setup :: Terminal -> IO ()
setup vte = do
  -- The default is to run the user's shell
  terminalForkCommand vte (Nothing :: Maybe String) (Nothing :: Maybe [String]) Nothing Nothing False False False

  terminalSetCursorBlinkMode vte CursorBlinkOff
  terminalSetScrollbackLines vte 12000
  terminalSetAudibleBell vte True

  -- This has to be first since it resets the others
  terminalSetColors vte fg bg palette

  terminalSetColorBold vte fg
  terminalSetColorHighlight vte fg


fg, bg :: Color
fg = Color 0xDDDD 0xCCCC 0xBBBB -- Foreground
bg = Color 0x0e0e 0x1111 0x1212 -- Background

palette :: [Color]
palette =
  -- “Photons” theme, slightly modified
  [ bg
  , Color 0xE0E0 0x3C3C 0x5252
  , Color 0x8585 0xC6C6 0x0000
  , Color 0xFFFF 0xA6A6 0x0000
  , Color 0x0000 0x9999 0xBCBC
  , Color 0xE5E5 0x1616 0x8888
  , Color 0x7F7F 0xC5C5 0xC6C6
  , Color 0xCCCC 0xCCCC 0xCCCC
  , Color 0x2E2E 0x3535 0x3737
  , Color 0xFFFF 0x5D5D 0x4A4A
  , Color 0xB3B3 0xFFFF 0x1D1D
  , Color 0xFFFF 0xAAAA 0x0F0F
  , Color 0x5C5C 0xDEDE 0xDEDE
  , Color 0xBEBE 0x4747 0x7373
  , Color 0x7676 0xB2B2 0xB3B3
  , Color 0xEEEE 0xEEEE 0xEEEE
  ]
