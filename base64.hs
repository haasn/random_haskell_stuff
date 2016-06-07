import Control.Lens
import Control.Monad

import Data.Bits.Lens (bits)
import Data.ByteString (ByteString)
import Data.ByteString.Lens (bytes)
import Data.List.Split.Lens (chunking)
import Data.Word (Word8)

charset :: Int -> Char
charset = (!!) $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"

base64enc :: ByteString -> String
base64enc = view $ chunking 6 (bytes.backwards bits).to enc
 where
  enc :: [Bool] -> String
  enc bs = charset (0 & partsOf (backwards $ taking 6 bits) .~ bs)
         : replicate (3 - length bs `div` 2) '='
