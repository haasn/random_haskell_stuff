import Control.Lens
import Control.Monad ((<=<), replicateM, liftM3)
import Control.Monad.Random

import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Word (Word8)

voronai :: [(Double, Double, a)] -> (Double, Double) -> a
voronai points d = minimumBy (comparing $ distance d) points ^. _3
  where distance (a,b) (c,d,_) = (a-c)^2 + (b-d)^2

render :: FilePath -> (Int, Int) -> ((Double, Double) -> Word8) -> IO ()
render p (w,h) f = runIL . writeImage p . Grey <=< computeP $
  fromFunction (ix2 h w) (\(Z:.i:.r) -> f (r/.w, i/.h))

main = render "voronai.png" (1920, 1280) . voronai =<< replicateM 100 rand
  where rand = liftM3 (,,) getRandom getRandom getRandom

(/.) :: (Integral a, Integral b, Fractional c) => a -> b -> c
x /. y = fromIntegral x / fromIntegral y
