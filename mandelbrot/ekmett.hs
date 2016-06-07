{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Codec.Compression.Zlib
import Control.Lens
import Control.Monad ((<=<))
import Data.Array.Repa hiding (traverse)
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Complex
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.List.Split.Lens (chunking)
import Data.Monoid
import qualified Data.Vector as V

-- * Mandelbrot

-- show
mandelbrot :: Int -> Int -> Int -> Lazy.ByteString
mandelbrot n w h = png w h $ \r i -> 
 greyscale $ maybe 0 scale $ steps $ (r/.w*3-2) :+ (i/.h*2-1)
 where 
   x /. y = fromIntegral x / fromIntegral y :: Double
   scale k = floor (k /. n * 255)
   greyscale x = (x,x,x)
   diverges (r :+ i) = r^2 + i^2 > 4
   steps c = iterate (\z -> z^2 + c) 0 ^?
     taking n ifolded.filtered diverges.asIndex
-- /show

-- * Folds

data L b a = forall x. L (x -> b -> x) x (x -> a)

more :: Lazy.ByteString -> L Word8 a -> a
more bs (L xbx x xa) = xa (Lazy.foldl' xbx x bs)

-- * CRC32

crc32 :: L Word8 Word32
crc32 = L step 0xffffffff complement where
  step r b = unsafeShiftR r 8 `xor` crcs Unboxed.! fromIntegral (xor r (fromIntegral b) .&. 0xff)

crcs :: Unboxed.Vector Word32
crcs = Unboxed.generate 256 (go.go.go.go.go.go.go.go.fromIntegral) where
  go c = unsafeShiftR c 1 `xor` if c .&. 1 /= 0 then 0xedb88320 else 0

-- * PNG

putChunk :: Lazy.ByteString -> Lazy.ByteString -> Put
putChunk h b = do
  putWord32be $ fromIntegral (Lazy.length b)
  putLazyByteString h
  putLazyByteString b
  putWord32be $ more (h <> b) crc32

png :: Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Lazy.ByteString
png w h p = runPut $ do
  putLazyByteString "\x89PNG\r\n\x1a\n"
  putChunk "IHDR" $ runPut $ do
    putWord32be (fromIntegral w)
    putWord32be (fromIntegral h)
    putWord8 8 -- 8 bit color depth
    putWord8 2 -- RGB
    putWord8 0
    putWord8 0
    putWord8 0
  putChunk "IDAT" $
    compressWith defaultCompressParams { compressLevel = bestSpeed } . runPut $ do
      putWord8 0
      a <- computeUnboxedP $ fromFunction (ix2 h w) (\(Z:.y:.x) -> p x y)
      let l = intersperse [putWord8 0] $ toList a^..chunking w (folded.to put)
      sequence_ (fmap sequence_ l)
  putChunk "IEND" mempty

main :: IO ()
main = Lazy.writeFile "ekmett.png" (mandelbrot 20 1920 1280)
