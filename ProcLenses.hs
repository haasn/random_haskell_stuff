{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Lens

import Data.List.Lens

foo = proc a -> do
  b <- _head -< a
  c <- _tail -< a
  returnA -< b
  --returnA -< runKleisli $ Kleisli b &&& Kleisli c
