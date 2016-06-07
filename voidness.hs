{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Control.Applicative
import Data.Void

newtype Bad a = Bad Void

bad :: Setter' (Bad a) Void
bad f (Bad x) = Bad <$> f x

evil :: Void -> Bad ()
evil = Bad

k0ral :: Monad m => ASetter' a b -> (b -> m ()) -> a -> m a
k0ral = undefined
