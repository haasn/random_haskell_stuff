{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.TH (makeLenses)

data Old a = Old
  { foo :: Int
  , bar :: a
  , bat :: String
  }

data New a = New
  { _qluux :: Int
  , _quack :: a
  , _frob  :: String
  }

makeLenses ''New

-- Some example code, old and new

defOld :: Old ()
defOld = Old
  { foo = 5
  , bar = ()
  , bat = "old"
  }

defNew :: New ()
defNew = New{}
  |> qluux <~ 7
  |. quack <~ ()
  |. frob  <~ "new"

myOld :: Old Char
myOld = defOld { bar = 'x' }

myNew :: New Char
myNew = defNew |> quack <~ 'y'

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(|.) :: (b -> c) -> (a -> b) -> a -> c
(|.) = (.)
infixr 3 |.
