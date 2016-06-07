{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.TH (makeLenses)
import Control.Monad.State (State)

-- So far so good
data Foobar = Foobar
  { _foo :: Int
  , _bar :: String
  , _bat :: Bool
  }

makeLenses ''Foobar

-- Possibly generate this with TH?
newFoobar = Foobar
  { _foo = error "foo was left uninitialized"
  , _bar = error "bar was left uninitialized"
  , _bat = error "bat was left uninitialized"
  }

-- Alias for horribleness
this :: Simple Lens a a
this = id

-- Imperative code
test = do
  this .= newFoobar

  this.foo .= 3
  this.bar .= "batbar"
  this.bat .= True

  return ()
