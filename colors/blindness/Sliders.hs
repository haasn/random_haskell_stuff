import Control.Monad
import Control.Monad.Trans
import Data.Array.MArray
import Data.Word

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

draw :: Surface -> Double -> Render ()
draw original x = do
  -- Clear the whole thing
  setSourceRGB 0 0 0
  paint

  setSourceSurface original 0 0
  paint

  -- Redirect drawing calls to temporary surface
  pushGroup
  setSourceRGB 0 0 0
  paint

  -- Paint the original, but mirrored horizontally
  scale (-1) 1
  translate (-200) 0
  setSourceSurface original 0 0
  paint

  -- Multiply it by factor (1-x)
  setSourceRGB (1-x) (1-x) (1-x)
  setOperator OperatorMultiply
  paint

  -- Draw this temporary surface back to the main drawing area, with OpAdd
  popGroupToSource
  setOperator OperatorAdd
  paint

  -- Draw the original below for reference
  identityMatrix
  translate 0 200
  setSourceSurface original 0 0
  setOperator OperatorOver
  paint

-- Main program

main = do
  initGUI
  window     <- windowNew
  vbox       <- vBoxNew False 2
  adjustment <- adjustmentNew 1 0 1 0.1 0.1 0
  scale      <- hScaleNew adjustment
  canvas     <- drawingAreaNew

  original   <- imageSurfaceCreateFromPNG "test.png"

  scaleSetDigits scale 2

  set window [ containerChild := vbox   ]
  set vbox   [ containerChild := scale
             , containerChild := canvas ]

  on scale valueChanged $ widgetQueueDraw canvas

  on canvas exposeEvent $ do
    w <- eventWindow
    x <- liftIO $ get scale rangeValue

    liftIO . renderWithDrawable w $ draw original x
    return True

  window `onDestroy` mainQuit

  widgetShowAll window
  mainGUI
