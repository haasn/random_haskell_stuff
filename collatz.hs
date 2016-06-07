{-# LANGUAGE BangPatterns #-}

import Data.Array.Repa as R

main = foldAllP max' (0,0) arr >>= print
  where arr = fromFunction (ix1 1000000) (\(_:.n) -> (n, step n 1))

max' :: (Int, Int) -> (Int, Int) -> (Int, Int)
max' x@(_, n) y@(_, m)
  | m > n     = y
  | otherwise = x

step :: Int -> Int -> Int
step 0 _ = 0
step 1 n = n
step n !s
  | even n    = step (n `quot` 2) (s+1)
  | otherwise = step (3*n + 1) (s+1)
