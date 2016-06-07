import Criterion.Main

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs
foldl' _ z []     = z

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ z []     = z
foldl'' f z (x:xs) = let z' = f z x in z' `seq` foldl'' f z' xs

main = defaultMain
  [ bench "foldl'"  $ whnf (foldl'  const 0) [1..1000000 :: Int]
  , bench "foldl''" $ whnf (foldl'' const 0) [1..1000000 :: Int]
  ]
