import Data.List (partition)

import Control.Lens
import Control.Monad (join)
import Control.Parallel.Strategies

qsort []     = []
qsort (x:xs) = sort l ++ [x] ++ sort g
  where (l,g) = partition (<x) xs `using` join parTuple2 rseq

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge xp@(x:xs) yp@(y:ys)
  | x <= y    = x : merge xs yp
  | otherwise = y : merge xp ys



main = print $ sort [1..10^4]
