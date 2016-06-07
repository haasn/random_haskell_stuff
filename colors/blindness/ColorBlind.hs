import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI

  window <- windowNew
  onDestroy window mainQuit

  button <- buttonNew
  set button [ buttonLabel := "Hello World" ]
  onClicked button (putStrLn "Hello World")
  set window [ containerChild := button ]

  widgetShowAll window
  mainGUI
