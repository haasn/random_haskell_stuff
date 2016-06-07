{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Fix

import Data.Function (fix)


-- ??
-- instance MonadFix m => MonadFix (ContT r m) where
  -- mfix :: (a -> ContT r m a) -> ContT r m a
  -- mfix f g = f (

-- cfix :: MonadFix m => (a -> ContT r m a) -> ContT r m a
-- cfix f = lift $ mfix (flip runContT return . f)

cfix :: (a -> (a -> r) -> r)  ->  (a -> r) -> r
cfix f 

label :: MonadCont m => m (m a)
label = callCC $ return . fix

test :: ContT r IO ()
test = let o = liftIO . putStrLn in do
  o "before jump"

  -- cfix (>> label)

  o "after jump"

{-
test :: ContT r IO ()
test = let o = liftIO . putStrLn in do { rec { rec
  o "hello, I am going to jump into the future" ;
  future ;

  o "this will never be reached" ;

  future <- label ;
  o "this is in the future!" } ;
  return () }
-}
