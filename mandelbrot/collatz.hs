import Control.Monad ((<=<))
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.Complex
import Data.Word (Word8)

iters :: Int
iters = 200

mandelbrot :: RealFloat a => Complex a -> Word8
mandelbrot c = go c iters
  where go _ 0 = 0
        go z@(r :+ i) n
          | isNaN (r^2 + i^2) = floor (n /. iters * 255)
          | otherwise = go (0.25 * (1 + 4*z - (1+2*z)*cos (pi*z))) (n-1)

render :: FilePath -> (Int, Int) -> (Complex Double -> Word8) -> IO ()
render p (w,h) f = runIL . writeImage p . Grey <=< computeP $
  fromFunction (ix2 h w) (\(Z:.i:.r) -> f $ (r/.w*3-2) :+ (i/.h*2-1))

main = render "collatz.png" (1920, 1280) mandelbrot

(/.) :: (Integral a, Integral b, Fractional c) => a -> b -> c
x /. y = fromIntegral x / fromIntegral y
