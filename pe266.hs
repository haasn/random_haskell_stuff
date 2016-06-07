import qualified Data.Set as S
import Data.List (foldl')

import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Squares

pe266 :: Integer -> [Integer] -> S.Set Integer
pe266 n = foldl' step (S.singleton 1)
  where step s p = S.union s $ S.map (f p) s
        f p x | p*x > n = x | otherwise = p*x

solution :: Integer -> Integer
solution n = S.findMax $ pe266 p xs
  where xs = takeWhile (<n) primes
        p  = integerSquareRoot' $ product xs

main = print $ solution 100 `rem` 10^16
