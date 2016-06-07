{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free
import Control.Monad

data Card = Card
  { name   :: String
  , cost   :: Game -> Int
  , effect :: Effect ()
  }

instance Show Card where
  show (Card n _ _) = n

type Context = ()

data Game = Game
  { player :: Player
  , enemy  :: Player
  }
  deriving Show

data Player = Player
  { unit     :: Character
  , hand     :: [Card]
  , board    :: [Minion]
  , mana     :: Int
  , maxMana  :: Int
  , overload :: Int
  }
  deriving Show

data Character = Char
  { health    :: Int
  , maxHealth :: Int
  , shield    :: Int
  , attack    :: Int
  , isMinion  :: Bool
  , isBeast   :: Bool
  , isDemon   :: Bool
  , isMurloc  :: Bool
  , hasDivine :: Bool
  }
  deriving Show

-- Per convention, a Minion has isMinion set to True
type Minion = Character

data EffectF n
  = Invalid
  | Location (Context -> n)
  | Target (Character -> n)
  | Read (Game -> n)
  | Write Game n
  deriving Functor

type Effect = Free EffectF

invalid :: Effect ()
invalid = liftF Invalid

require :: Bool -> Effect ()
require c = unless c invalid

location :: Effect Context
location = liftF (Location id)

target :: Effect Character
target = liftF (Target id)

minion :: Effect Minion
minion = do
  t <- target
  require (isMinion t)
  return t

damage :: Int -> Character -> Effect ()
damage d c
  | 

-- Test values

testGame = Game testPlayer testPlayer

testPlayer = Player
  { unit     = Char 30 30 0 0 False False False False False
  , hand     = []
  , board    = []
  , mana     = 1
  , maxMana  = 1
  , overload = 0
  }

testCard = Card "Test Spell" 1 $ minion >>= damage 3
