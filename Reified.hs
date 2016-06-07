{-# LANGUAGE ScopedTypeVariables, TypeOperators, ConstraintKinds, FlexibleContexts, RankNTypes #-}

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Proxy
import Data.Reflection
import Data.Tagged

-- General machinery

class Reified a where
  reflect' :: a

instance Reifies s a => Reified (Tagged s a) where
  reflect' = Tagged $ reflect (Proxy :: Proxy s)

using :: a -> (Reified a => b) -> b
using c a = reify c $ \(_ :: Proxy s) -> a \\ trans
  (unsafeCoerceConstraint :: Reified (Tagged s a) :- Reified a)
  (Sub Dict :: Reifies s a :- Reified (Tagged s a))

-- Particular example

data Config = Config { _string :: String, _delay :: Int }
type Conf = Reified Config

string :: Conf => String
string = _string reflect'

delay :: Conf => Int
delay = _delay reflect'

run :: Conf => IO ()
run = forever $ do
  -- No plumbing or phantom types
  putStrLn string
  threadDelay delay

main = do
  -- Passed to forkIO with no further ado
  using (Config "other" 200000 ) (forkIO run)
  using (Config "this"  1000000) run
