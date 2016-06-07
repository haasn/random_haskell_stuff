import Numeric.Lens (binary)
import Control.Lens ((#))
import Data.List (foldl')

multiply :: (Ord a, Integral a) => a -> a -> a
multiply a b | b < 0 = - go (-b)
             | otherwise = go b
    where go 0 = 0
          go b | odd b     = a + r + r
               | otherwise = r + r
               where r = go (b `quot` 2)

mulFold :: Integral a => a -> a -> a
mulFold a b = go (binary # b)
  where go ('-':r) = - go r
        go r = foldl' f 0 r
        f r '0' = r+r
        f r '1' = a+r+r

-- Testing
propMul m a b = a * b == m a b
propBound m a = propMul m a (minBound :: Int) && propMul m a (maxBound :: Int)
