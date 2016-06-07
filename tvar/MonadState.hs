{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.STM           (atomically)

instance (MonadReader (TVar a) m, MonadIO m) => MonadState a m where
  state f = do
    tvar <- ask
    liftIO . atomically $ do
      s <- readTVar tvar
      let (a, s') = f s
      writeTVar tvar s'
      return a

-- Usage examples

type Foo = ReaderT (TVar Int) IO

add1 :: Foo ()
add1 = modify (+1)

foo :: Foo String
foo = do
  a <- get
  add1
  b <- get

  return $ show a ++ "; " ++ show b

main :: IO ()
main = do
  t <- newTVarIO 0
  print =<< runReaderT foo t
