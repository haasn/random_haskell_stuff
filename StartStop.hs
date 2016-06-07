import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)

import System.IO

data Event = KeyEvent Char | TimerEvent
  deriving Show

forkEvent :: IO a -> IO (STM a)
forkEvent a = do
  v <- newEmptyTMVarIO
  forkIO . forever $ atomically . putTMVar v =<< a
  return (takeTMVar v)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  k <- forkEvent getChar
  t <- forkEvent (threadDelay 100000)

  forever $ do
    e <- atomically $ (KeyEvent <$> k) <|> (TimerEvent <$ t)
    print e
