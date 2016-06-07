import Criterion.Main
import Control.Monad.ST
import Control.Monad.Identity
import Data.STRef
import Data.Array.Repa

fac_rec, fac_iter :: Int -> Int

fac_rec 1 = 1
fac_rec n = n * fac_rec (n-1)

fac_iter x = runST $ do
  r <- newSTRef 1
  let go 1 = return ()
      go i = modifySTRef' r (*i) >> go (i-1)
  go x
  readSTRef r

fac_repa :: Int -> Int
fac_repa n = foldAllS (*) 1 $ fromFunction (Z:.n) (\(Z:.i) -> i)

fac_par :: Monad m => Int -> m Int
fac_par n = foldAllP (*) 1 $ fromFunction (Z:.n) (\(Z:.i) -> i)

main = defaultMain
  [ bench "fac_rec"  $ whnf fac_rec  50
  , bench "fac_iter" $ whnf fac_iter 50
  , bench "fac_repa" $ whnf fac_repa 50
  , bench "fac_par"  $ (fac_par 50 :: IO Int)
  ]
