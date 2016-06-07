import System.Environment (getArgs)

-- part of JuicyPixels
import Codec.Picture

-- Luminosity of a color normalized to 0-1, multiplied by alpha as well to simulate a “background” of black
luma :: PixelRGBA8 -> Double
luma (PixelRGBA8 r g b a) = fromIntegral a / 255 * (0.2126 * fromIntegral r / 255 + 0.7152 * fromIntegral g / 255 + 0.0722 * fromIntegral b / 255)

-- Surface density of point (x,y) on the planar lamina given by the image
density :: Image PixelRGBA8 -> Int -> Int -> Double
density = luma .:: pixelAt

-- Surface mass of an image
mass :: Image PixelRGBA8 -> Double
mass = integrateImage density

-- x and y moments
xMoment :: Image PixelRGBA8 -> Double
xMoment = integrateImage (\image x y -> fromIntegral y * density image x y)

yMoment :: Image PixelRGBA8 -> Double
yMoment = integrateImage (\image x y -> fromIntegral x * density image x y)

-- Center of mass
center :: Image PixelRGBA8 -> (Double, Double)
center image = (my / m, mx / m)
  where mx = xMoment image
        my = yMoment image
        m  = mass    image

-- Helper function: Integral, with steps of 1
integrate :: (Enum x, Num y) => (x -> y) -> x -> x -> y
integrate f min max = sum [f x | x <- [min .. max]]

-- Integrate over an entire image
integrateImage :: Num b => (Image a -> Int -> Int -> b) -> Image a -> b
integrateImage f image = integrate (\y -> integrate (\x -> f image x y) 0 xmax) 0 ymax
  where xmax = imageWidth  image - 1
        ymax = imageHeight image - 1

-- Helper function: (.::)
(.::) = (.).(.).(.)

-- Helper function: Get the center of a dynamic image
-- FIXME: Make it work for non-RGBA8 images
center' :: DynamicImage -> (Double, Double)
center' (ImageRGBA8 i) = center i


-- Main function:
main = getArgs >>= mapM_ output
  where output :: String -> IO ()
        output f = readImage f >>= print . fmap center'
