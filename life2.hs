{-# LANGUAGE TemplateHaskell #-}

import Control.Comonad
import Control.Lens
import Data.Array

type Board = Store (Int, Int) Bool

blank :: Board
