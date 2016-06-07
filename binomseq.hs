import Data.List

f :: String -> Int
f s = f 0
  where ns     = nub s
        groups = group $ sort s
        f n    = sum [ binom (length (filter (/=c) ns)) (3-n)
                     | s@(c:_) <- groups
                     , length s >= n
                     ]

binom :: (Integral a, Num b) => a -> a -> b
binom n m = fromIntegral $ product [n-m+1..n] `div` product [1..m]
