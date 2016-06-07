{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Tardis
import Control.Monad.Trans.Class

newtype ContextT c a m b = ContextT { unContextT :: TardisT c c m b }
  deriving (Functor, Applicative, Monad, MonadTrans)

contextT :: MonadFix m => (a -> c -> c) -> (a -> (c, c) -> m b) -> a -> ContextT c a m b
contextT u f a = ContextT $ do
  c <- tardis $ \(~(n,p)) -> ((p,n), (u a n, u a p))
  lift $ f a c

evalContextT :: Monad m => ContextT c a m b -> (c,c) -> m b
evalContextT = evalTardisT . unContextT

-- Identity version

type Context c a = ContextT c a Identity

context :: (a -> c -> c) -> (a -> (c, c) -> b) -> a -> Context c a b
context u f a = ContextT . tardis $ \(~(n,p)) -> (f a (p,n), (u a n, u a p))

evalContext :: Context c a b -> (c,c) -> b
evalContext c = runIdentity . evalContextT c

{-

> evalContext ?? ([],[]) $ traverse (context (:) $ \a (p,n) -> (a `elem` p, a `elem` n)) [1,2,3,2,1]
[(False,True),(False,True),(False,False),(True,False),(True,False)]

-}
