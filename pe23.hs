import Control.Lens
import Data.IntSet as IS
import Math.NumberTheory.Primes (isPrime)

divisors :: Int -> IntSet
divisors n = go 2
 where
  go d
    | n == 1 || isPrime (fromIntegral n) = singleton 1
    | r == 0    = let x = divisors n' in insert n' $ IS.map (*d) x `union` x
    | otherwise = go (d+1)
    where (n',r) = n `quotRem` d

abundant :: Int -> Bool
abundant n = foldl' (+) 0 (divisors n) > n

abundants :: IntSet
abundants = IS.filter abundant $ fromDistinctAscList [1..21822]

pe23 :: Int -> Bool
pe23 n = none (\x -> f x && f (n-x)) [1..n]
  where f = IS.member ?? abundants

main = print . sum $ Prelude.filter pe23 [1..21822]
