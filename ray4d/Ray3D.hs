{-# LANGUAGE ViewPatterns, RankNTypes, LiberalTypeSynonyms #-}

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.Array.Repa (computeP, fromFunction, ix2, Z(..), (:.)(..))
import Data.Array.Repa.IO.DevIL
import Data.Function (on)
import Data.List
import Data.Monoid hiding ((<>))
import Data.Semigroup hiding (First)
import Data.Word (Word8)

import Linear
import Linear.Plucker

-- Rays and polygons

type D2 = V2 Double
type D3 = V3 Double
type DP = Plucker Double
type Polygon = [DP]

ray :: D3 -> D3 -> DP
ray = plucker `on` point

vertices :: [D3] -> Polygon
vertices [] = []
vertices (map point -> x:xs) = zipWith plucker (x:xs) (xs++[x])

stabs :: DP -> Polygon -> Bool
stabs l = all (\v -> l >< v < 0)

-- Simple cameras

type View   = (->) DP
type Render = (->) D2
type Camera = Render DP

linear :: Camera
linear (V2 x y) = ray (V3 x y 0) (V3 x y 1)

-- Scenes

type Object a = (Polygon, a)

trivial :: [Object a] -> View [a]
trivial s d = [a | (p,a) <- s, d `stabs` p]

-- Test

tri :: Polygon
tri = vertices [V3 0.4 0.4 1, V3 0.6 0.4 1, V3 0.5 0.6 1]

square :: Polygon
square = vertices [V3 0.2 0.2 2, V3 0.8 0.2 2, V3 0.8 0.8 2, V3 0.2 0.8 2]

example :: View [Word8]
example = trivial [(square, 100), (tri, 200)]

test :: Render Word8
test = foldr const 0 . example . linear

-- Generate output

output :: FilePath -> (Int, Int) -> Render Word8 -> IO ()
output f (w,h) r = runIL . writeImage f . Grey <=< computeP $
  fromFunction (ix2 h w) (\(Z:.y:.x) -> r $ V2 (x/.w) (y/.h))
  where x /. y = fromIntegral x / fromIntegral y

main = output "test.png" (640, 640) test

-- custom Plucker implementation
-- math from http://tog.acm.org/resources/RTNews/html/rtnv10n3.html#art11

lineJoin :: Num a => Plucker a -> Plucker a -> V4 a
lineJoin (Plucker a b c d e f) (Plucker u v w _ _ _) =
  V4 (h `dot` g') x y z
  where (g,g',h) = (V3 a b c, V3 u v w, V3 d e f)
        V3 x y z = g `cross` g'
