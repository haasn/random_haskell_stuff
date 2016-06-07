import Codec.Picture
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.Tuple (swap)

main = forM_ [1..100000] $ \n -> do
  gen <- newStdGen
  let (_, img) = generateFoldImage f gen 8 8
      f g _ _  = swap $ runRand pixel g
      pixel    = gray <$> getRandom
      gray True  = 255
      gray False = 0
  when (n `rem` 1000 == 0) $ print n
  writeGifImage ("/mem/random" ++ show n ++ ".gif") img
