import Network

import Data.List (delete)
import Data.Tuple (swap)

import Control.Applicative
import Control.Monad (forever, unless)
import Control.Monad.State
import Control.Exception (finally)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar hiding (withMVar)

import GHC.IO.Handle

-- Dummy types to serve as an example

type MyMonad = StateT MyState IO
type MyState = [Handle]

defaultState :: MyState
defaultState = []

-- IO wrappers for underlying stateful computations

main :: IO ()
main = withSocketsDo $ do
  m <- newMVar defaultState
  sock <- listenOn (PortNumber 1234)

  (`finally` sClose sock) . forever $ do
    (h, host, port) <- accept sock
    hSetBuffering h LineBuffering

    putStrLn $ "Host " ++ show host ++ " connected on port " ++ show port

    withMVar m (insert h)
    forkIO $ runThread h m `finally` withMVar m (close h)

runThread :: Handle -> MVar MyState -> IO ()
runThread h m = do
  eof <- hIsEOF h
  unless eof $ do
    line <- hGetLine h
    withMVar m (process line)
    runThread h m

-- Stateful computations

process :: String -> MyMonad ()
process s = do
  liftIO $ putStrLn s
  hSendAll s

insert :: Handle -> MyMonad ()
insert = modify . (:)

close :: Handle -> MyMonad ()
close h = do
  liftIO $ hClose h
  modify $ delete h

hSendAll :: String -> MyMonad ()
hSendAll s = get >>= mapM_ (liftIO . (`hPutStrLn` s))

-- Helper functions

withMVar :: MVar MyState -> MyMonad a -> IO a
withMVar m a = modifyMVar m $ \st ->
  swap <$> runStateT a st

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h str = do
  hPutStr h str
  hPutChar h '\n'
  hFlush h
