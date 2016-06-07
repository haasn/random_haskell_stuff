{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

import Control.Monad

instance (Num b, a ~ Integer) => Num (a -> b) where
    fromInteger d a = fromInteger (a+d)

    -- The rest are just the standard applicative extension
    (+) = liftM2 (+)
    (-) = liftM2 (-)
    (*) = liftM2 (*)
    negate = liftM negate
    abs    = liftM abs
    signum = liftM signum

main = do
  print $ 1         -- 1
  print $ 1 2       -- 3
  print $ 1 2 3     -- 6
  print $ 1 2 3 4   -- 10
  print $ 1 2 3 4 5 -- 15

  let two = 2
  print $ two     -- 2
  print $ two + 5 -- 7
  print $ two 3   -- 5
  print $ two 3 5 -- 10
