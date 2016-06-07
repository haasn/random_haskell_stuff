{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Main where

import Control.Monad.Identity
import Control.Monad.Reader

import Data.Proxy
import Data.Reflection

-- Boring cruft

type Reify s = ReifyT s Identity

newtype ReifyT s m a = ReifyT { runReifyT :: Proxy s -> m a }

instance Monad m => Monad (ReifyT s m) where
  return = lift . return

  ReifyT r >>= f = ReifyT $ \s -> do
    a <- r s
    runReifyT (f a) s

instance MonadTrans (ReifyT s) where
  lift = ReifyT . const

instance MonadIO m => MonadIO (ReifyT s m) where
  liftIO = lift . liftIO

it :: (Reifies s a, Monad m) => ReifyT s m a
it = ReifyT (return . reflect)

using :: a -> (forall s. Reifies s a => Reify s r) -> r
using a = runIdentity . usingT a

usingT :: a -> (forall s. Reifies s a => ReifyT s m r) -> m r
usingT a r = reify a (runReifyT r)

-- Example program

main :: IO ()
main = do
    putStrLn "hello,"
    putStrLn "world!"

    v <- usingT 2 addTwo
    print v

type Example s = ReifyT s IO

getEnv :: Reifies s a => Example s a
getEnv = do
  liftIO $ putStrLn "it has been requested!"
  it

addTwo :: (Reifies s a, Num a, Show a) => Example s String
addTwo = do
  v <- getEnv
  liftIO $ putStrLn "getEnv has been added!"
  return $ show (v + v)
