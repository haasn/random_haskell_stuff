import Control.Arrow
import Control.Monad

import qualified Data.IntMultiSet as IMS
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Data.List

import Math.NumberTheory.Powers.Squares

type Sig = IMS.IntMultiSet

sig :: [Int] -> Sig
sig = IMS.fromList

groups :: [[Int]] -> [(Sig, [[Int]])]
groups = MM.assocs . MM.fromList . map (sig &&& id)

combinations :: Eq a => Int -> [a] -> [[a]]
combinations n = filter unique . replicateM n
  where unique xs = xs == nub xs

maps :: Sig -> [M.Map Int Char]
maps s = map (M.fromList . zip (IMS.distinctElems s))
          $ combinations (IMS.distinctSize s) "0123456789"

number :: Ord a => M.Map a Char -> [a] -> Int
number m = read . map (m M.!)

squares :: Sig -> [[Int]] -> [[Int]]
squares s xs = filter (all isSquare) . map (\m -> map (number m) xs) $ maps s

pe98 :: [String] -> Int
pe98 as = maximum . concat . concat . map (uncurry squares) $ groups xs
  where xs = map (map fromEnum) as

main = print . pe98 . read =<< readFile "words.txt"
