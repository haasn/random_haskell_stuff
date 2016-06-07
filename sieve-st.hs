import Control.Monad (when, forM_, filterM)
import Control.Monad.ST

import Data.Array.IArray (elems)
import Data.Array.ST

primesBelow :: Int -> [Int]
primesBelow n = runST $ do
  a <- newArray (2, n) True :: ST s (STUArray s Int Bool)
  forM_ [2..sqrt' n] $ \i -> do
    x <- readArray a i
    when x $ forM_ [i^2, i^2+i .. n] $ \j -> writeArray a j False
  filterM (readArray a) [2..n]
 where sqrt' = ceiling . sqrt . fromIntegral
