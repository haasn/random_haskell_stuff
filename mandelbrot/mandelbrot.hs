import Control.Monad ((<=<))
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.Complex
import Data.Word (Word8)

iters :: Int
iters = 30

mandelbrot :: RealFloat a => Complex a -> Word8
mandelbrot c = go 0 iters
  where go _ 0 = 0
        go z@(r :+ i) n
          | r^2 + i^2 > 4 = floor (n /. iters * 255)
          | otherwise = go (z^2 + c) (n-1)

render :: FilePath -> (Int, Int) -> (Complex Double -> Word8) -> IO ()
render p (w,h) f = runIL . writeImage p . Grey <=< computeP $
  fromFunction (ix2 h w) (\(Z:.i:.r) -> f $ (r/.w*3-2) :+ (i/.h*2-1))

main = render "mandelbrot.png" (1920, 1280) mandelbrot

(/.) :: (Integral a, Integral b, Fractional c) => a -> b -> c
x /. y = fromIntegral x / fromIntegral y
