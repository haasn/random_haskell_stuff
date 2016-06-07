{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Control.Applicative
import Diagrams.Backend.Gtk
import Diagrams.TwoD.Text
import Graphics.UI.Toy
import Graphics.UI.Toy.Gtk

instance Interactive Gtk String
instance GtkDisplay String where
  -- doesn't work :(
  display d _ s = s <$ defaultRender d (text s)

main = runToy "Hello, world!"
