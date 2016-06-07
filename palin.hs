import Control.Lens
import Data.Char (digitToInt)
import Test.QuickCheck

-- Is a number a palindrome?
isPalin n = show n == reverse (show n)

-- Number of palindromes of exactly length n
palin10 n = 9 * 10 ^ ((n-1) `quot` 2)

-- Number of palindromes below n
palinLim n = base + rest + errors
    where s = show n
          l = length s
          -- All palindromes strictly below n's length (easy to compute)
          base = sum [palin10 n | n <- [1..l-1]]
          -- All palindromes of length l, additional requirement that p <= n
          half = take ((l+1) `div` 2) s
          rest = read (half & _head %~ pred)
          -- Account for two errors with the above estimations:
          -- 1. handling of 0 (it's also a palindrome)
          -- 2. is half ++ reverse half below n or not?
          errors = 1 + case compare s (half ++ reverse (take (l`div`2) s)) of
                           LT -> 0
                           _  -> 1

-- Testing
propPalin n = (n > 0 && n < 100000) ==>
                  palinLim n == length [ p | p <- [0..n], isPalin p ]
