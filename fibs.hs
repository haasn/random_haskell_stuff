import Data.List (scanl)
import Criterion.Main

fibs :: [Int]
fibs = scanl (+) 0 (1 : fibs)

main = defaultMain [bench "fibs !! 42" $ whnf (fibs !!) 42]
