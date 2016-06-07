import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.List (partition)

radix' :: [[Bool]] -> [[Bool]]
radix' xs = recurse l ++ recurse h
  where (h, l)  = partition head xs
        recurse = partsOf (traverse._tail) %~ radix'

radix :: (Num a, Bits a) => [a] -> [a]
radix = partsOf (traverse.partsOf (backwards bits)) %~ radix'
