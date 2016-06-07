{-# LANGUAGE TypeFamilies #-}

import Control.Applicative (pure, liftA2)

class Predicate t where
  type Result t
  tt :: t -> [Result t]

instance Predicate Bool where
  type Result Bool = Bool
  tt = pure

-- Ugh, I wish we had a better way of getting “listable domains” without
-- resorting to Enum/Bounded
instance (Predicate t, Enum a, Bounded a) => Predicate (a -> t) where
  type Result (a -> t) = (a, Result t)
  tt p = [minBound .. maxBound] >>= liftA2 map (,) (tt . p)
