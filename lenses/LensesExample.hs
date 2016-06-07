{-# LANGUAGE TemplateHaskell, Rank2Types, LiberalTypeSynonyms #-}

import Control.Lens
import Control.Lens.TH

import Control.Monad.State

-- Simple data type for a person

data Person = Person
  { _name     :: String
  , _age      :: Int
  , _favColor :: String
  } deriving Show

makeLenses ''Person

-- Some examples

data People = People
  { _foo :: Person
  , _bar :: Person
  }

makeLenses ''People

calcAges :: State People Int
calcAges = do
  a <- use $ foo.age
  b <- use $ bar.age
  return(a+b)

both :: Simple Setter People Person
both = sets (\f (People a b) -> People (f a) (f b))

names :: Simple Setter [People] String
names = mapped.both.name
