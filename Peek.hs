import Control.Applicative

import GHC.IO.Handle
import GHC.IO.Handle.FD (stdin)

import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)

hPeekChar :: Handle -> IO (Maybe Char)
hPeekChar h = with '\0' $ \p -> do
  n <- hGetBufNonBlocking h p 1
  case n of
    0 -> return Nothing
    1 -> Just <$> peek p

peekChar :: IO (Maybe Char)
peekChar = hPeekChar stdin
