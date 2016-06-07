import Codec.Picture
import qualified Data.ByteString as BS
import Data.Word

-- The transformations I'm interested in

transform :: Color -> Color
transform (r, g, b) = let
    y = 0.299 * r + 0.587 * g + 0.114 * b
    u = 0.492 * (b - y)
    v = 0.877 * (r - y)

    u' = u * factorU
    v' = v * factorV + 0.03

    r' = y + v' / 0.877
    g' = y - 0.5808092 * v' - 0.394731375 * u'
    b' = y + u' / 0.492

  in clamp (r', g', b')

  where
    factorU = 1
    factorV = 0.2

-- My own color type for sanity

type Color = (Double, Double, Double)
type Color8 = (Word8, Word8, Word8)

to8 :: Color -> Color8
to8 (r,g,b) = (floor (r * 255), floor (g * 255), floor (b * 255))

from8 :: Color8 -> Color
from8 (r,g,b) =
  (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

clamp :: Color -> Color
clamp (r, g, b) = (c r, c g, c b)
  where c = max 0 . min 1

-- Functions to map over a dynamic image

mapDynamic :: (Color -> Color) -> DynamicImage -> DynamicImage
mapDynamic f (ImageRGB8 img) = ImageRGB8 $ pixelMap (lift f) img
  where lift f (PixelRGB8 r g b) =
          let (r', g', b') = to8 . f . from8 $ (r, g, b)
          in  PixelRGB8 r' g' b'

mapDynamic f (ImageRGBA8 img) = ImageRGBA8 $ pixelMap (lift f) img
  where lift f (PixelRGBA8 r g b a) =
          let (r', g', b') = to8 . f . from8 $ (r, g, b)
          in  PixelRGBA8 r' g' b' a

mapDynamic _ _ = error "Unsupported pixel type"

-- Main program action

main :: IO ()
main = getImage >>= putImage . mapDynamic transform
  where
    getImage = (either error id . decodeImage) `fmap` BS.getContents
    putImage = BS.putStr . either error id . encodeDynamicPng
