import Graphics.Gloss
import Graphics.Gloss.Raster.Field

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

main :: IO ()
main = display (InWindow "Gradient" (1000, 200) (10, 10)) black gradient

gradient :: Picture
gradient = makePicture 1000 200 1 1 go
  where
    go (xraw, yraw) = let (x, y) = ((xraw + 1) / 2, (yraw + 1) / 2) in
      fromColour $ (RGB (1-x) (1-x) x)

    fromColour (RGB r g b) = rgb r g b

saturate :: RGB Float -> RGB Float
saturate c = hsv (hue c) (saturation c) 1
