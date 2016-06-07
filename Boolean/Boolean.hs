{-# LANGUAGE FlexibleInstances #-}

import           Prelude hiding ((&&), (||), not)
import qualified Prelude as P

import Control.Applicative

class Boolean b where
  (&&), (||)  :: b -> b -> b
  not         :: b -> b
  true, false :: b

instance Boolean Bool where
  (&&)  = (P.&&)
  (||)  = (P.||)
  not   = P.not
  true  = True
  false = False

instance (Applicative f, Boolean b) => Boolean (f b) where
  (&&)  = liftA2 (&&)
  (||)  = liftA2 (||)
  not   = liftA not
  true  = pure true
  false = pure false
