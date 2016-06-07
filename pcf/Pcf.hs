{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.Bits
import Data.Bits.Lens
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Default
import Data.Int

main :: IO Pcf
main = decodeFile "ter-u14n.pcf"

-- Utility functions

getInt32 :: Get Int32
getInt32 = fromIntegral <$> getWord32le

putInt32 :: Int32 -> Put
putInt32 = putWord32le . fromIntegral

-- Internal types

data Type = TProperties | TAccelerators | TMetrics | TBitmaps | TInkMetrics
          | TBDFEncodings | TSWidths | TGlyphNames | TBDFAccelerators
  deriving (Show, Enum)

instance Binary Type where
  put t = putInt32 (1 `shiftL` fromEnum t)
  get   = toEnum . (^?! bits.filtered id.asIndex) <$> getInt32

-- External types

data Properties = Properties
  {
  }

instance Binary Properties where
  put = undefined

  get = do
    f <- getInt32
    error "fuck this"

data Pcf = Pcf
  { _properties :: Maybe Properties
  }
makeLenses ''Pcf

instance Default Pcf where
  def = Pcf Nothing


instance Binary Pcf where
  put = undefined

  get = do
    "\1fcp" <- replicateM 4 get
    tc <- getInt32
    ts <- replicateM (fromIntegral tc) $
            (,,,) <$> get <*> getInt32 <*> getInt32 <*> getInt32

    let f (p,pcf) (t,_,s,o) = do -- format is handled in each table separately
          skip $ fromIntegral (o-p)
          (pcf &) <$> case t of
            TProperties -> g properties
            _ -> return id
          return (p+s,pcf)

        g l = set l . Just <$> get

    snd <$> foldM f (8+tc*16, def) ts
