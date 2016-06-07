import Control.Applicative

import Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.IO.DevIL

import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)

newtype Stupid = Stupid { image :: Array F DIM2 Word8 }

instance Binary Stupid where
  get = do
    h <- get
    w <- get
    Stupid . copyS . R.map inv . fromByteString (ix2 h w)
      <$> getByteString (h*w)

  put (Stupid img) = do
    let (Z:.h:.w) = extent img
    put h
    put w
    mapM_ put $ toList img

inv :: Word8 -> Word8
inv x = 255 - x

readStupid :: FilePath -> IO Stupid
readStupid = fmap (\(Grey i) -> Stupid i) . runIL . readImage

writeStupid :: FilePath -> Stupid -> IO ()
writeStupid p (Stupid i) = runIL $ writeImage p (Grey i)

encode :: IO ()
encode = readStupid "mandelbrot.png" >>= encodeFile "mandelbrot.stupid"

decode :: IO ()
decode = decodeFile "mandelbrot.stupid" >>= writeStupid "stupid.png"
