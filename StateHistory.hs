{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances  #-}
module Control.Monad.State.History where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader   (MonadReader(..))
import           Control.Monad.State    (MonadState(..))
import           Control.Monad.Trans    (MonadTrans(..))
import           Control.Monad.Writer   (MonadWriter(..))

newtype HistoryT s m a = HistoryT { runHistoryT' :: s -> m (a, s, [s]) }

runHistoryT :: Functor m => HistoryT s m a -> s -> m (a, [s])
runHistoryT x = fmap (\(a, n, ns) -> (a, n : ns)) . runHistoryT' x

evalHistoryT :: Functor m => HistoryT s m a -> s -> m a
evalHistoryT x = fmap fst . runHistoryT x

execHistoryT :: Functor m => HistoryT s m a -> s -> m [s]
execHistoryT x = fmap snd . runHistoryT x

-- Monad instance

instance Monad m => Monad (HistoryT s m) where
  return a = HistoryT $ \s -> return (a, s, [])

  x >>= f = HistoryT $ \s -> do
    (a, u, us) <- runHistoryT' x s
    (b, v, vs) <- runHistoryT' (f a) u
    return (b, v, vs ++ us)

instance Functor m => Functor (HistoryT s m) where
  fmap f (HistoryT x) = HistoryT $ \s ->
    fmap (\(a, s, ss) -> (f a, s, ss)) (x s)

-- mtl class instances

instance MonadTrans (HistoryT s) where
  lift x = HistoryT $ \s -> x >>= \a -> return (a, s, [])

instance Monad m => MonadState s (HistoryT s m) where
  get     = HistoryT $ \s -> return (s, s, [])
  put new = HistoryT $ \s -> return ((), new, [s])

-- Instances for other mtl classes

{-

instance MonadWriter w m => MonadWriter w (HistoryT s m) where
  tell = lift . tell

instance MonadReader r m => MonadReader r (HistoryT s m) where
  ask = lift ask

instance MonadError e m => MonadError e (HistoryT s m) where
  throwError = lift . throwError

-}

-- Non-transformer variant

type History s = HistoryT s Identity

runHistory :: History s a -> s -> (a, [s])
runHistory x = runIdentity . runHistoryT x

evalHistory :: History s a -> s -> a
evalHistory x = runIdentity . evalHistoryT x

execHistory :: History s a -> s -> [s]
execHistory x = runIdentity . execHistoryT x
