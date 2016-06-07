{-# LANGUAGE RankNTypes, GADTs #-}

import Control.Lens
import Control.Category

import Data.Traversable

import Prelude hiding (id, (.))

aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside Prismoid = id
aside (Prism bt seta) = prism (fmap bt) $ \(e,s) -> case seta s of
  Left t -> Left (e, t)
  Right a -> Right (e, a)

aside' :: Traversable f => APrism s t a b -> Prism (f s) (f t) (f a) (f b)
aside' Prismoid = id
aside' (Prism bt seta) = prism (fmap bt) $ undefined
