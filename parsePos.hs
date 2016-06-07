import Control.Applicative

import Data.ByteString
import Data.Fix
import Data.Functor.Compose
import Data.Map

import Data.Binary.Get

-- B-encoded values
data B x
  = S !ByteString           -- String literal
  | I !Integer              -- Integer literal
  | Dict (Map ByteString x) -- Key/Value pairs
  | List [x]                -- List of values

-- Store source position that can be used to extract the original slice
data Slice x = Slice { source :: !ByteString, val :: !x }

-- Value type returned by the parser
type Value = Fix (Compose Slice B)

slice :: B Value -> ByteString -> Value
slice b bs = Fix . Compose $ Slice bs b

string :: Get Value
string =

-- Auxiliary functions

getAscii :: Get Char
getAscii = chr . fromIntegral <$> getWord8

getUntil :: Eq a => a -> Get a -> Get [a]
getUntil e g = do
  x <- g
  if x == a then
