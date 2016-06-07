{-# LANGUAGE BangPatterns #-}

import Data.Ratio
import Data.List

timestamps :: Rational -> [Integer]
timestamps r = map round [0, 1000/r ..]

differences :: Num a => [a] -> [a]
differences ts = zipWith (-) (drop 1 ts) ts

averages :: Integer -> [Integer] -> [Rational]
averages n = map ((%n) . sum . genericTake n) . tails

minmax :: Ord a => [a] -> (a, a)
minmax (x:xs) = foldl' f (x,x) xs
  where f (!mi,!ma) n = (min mi n, max ma n)

solve :: Int -> Integer -> Rational -> (Rational, Rational)
solve m n = minmax . take m . averages n . differences . timestamps
