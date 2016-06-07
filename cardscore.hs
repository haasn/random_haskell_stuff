{-# LANGUAGE ViewPatterns #-}

import Data.List (subsequences)

score :: (Num a, Eq a) => [a] -> a
score = sum . map points . subsequences
  where points [x,y] | x == y = 1
        points (sum -> 15) = 1
        points _ = 0
