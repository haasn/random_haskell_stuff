import Control.Lens
import Data.Char (toUpper)
import Data.Text.Lens (text)
import qualified Data.Text as T
import Criterion.Main

f,g,h :: T.Text -> T.Text
f = text %~ toUpper
g = setting T.map %~ toUpper
h x = T.map toUpper x
j = setting (setting T.map %~) %~ toUpper

main = t `seq` defaultMain
  [ bench "text"     $ whnf f t
  , bench "setting"  $ whnf g t
  , bench "T.map"    $ whnf h t
  , bench "setting2" $ whnf j t
  ]
  where t = T.pack . concat . replicate 10000 $ ['a'..'z'] ++ ['A'..'Z']

{-
warming up
estimating clock resolution...
mean is 878.3216 ns (640001 iterations)
found 1059181 outliers among 639999 samples (165.5%)
  524157 (81.9%) low severe
  535024 (83.6%) high severe
estimating cost of a clock call...
mean is 21.89030 ns (6 iterations)

benchmarking text
mean: 27.65251 ms, lb 27.62577 ms, ub 27.69394 ms, ci 0.950
std dev: 168.0779 us, lb 121.1902 us, ub 239.8833 us, ci 0.950

benchmarking setting
mean: 19.99330 ms, lb 19.97817 ms, ub 20.01674 ms, ci 0.950
std dev: 95.29721 us, lb 69.64924 us, ub 133.0983 us, ci 0.950

benchmarking T.map
mean: 20.14055 ms, lb 20.10308 ms, ub 20.27841 ms, ci 0.950
std dev: 326.6213 us, lb 81.69428 us, ub 749.7589 us, ci 0.950
found 11 outliers among 100 samples (11.0%)
  5 (5.0%) high mild
  6 (6.0%) high severe
variance introduced by outliers: 9.406%
variance is slightly inflated by outliers

benchmarking setting2
mean: 20.11641 ms, lb 20.09896 ms, ub 20.14263 ms, ci 0.950
std dev: 107.5442 us, lb 78.76399 us, ub 143.6313 us, ci 0.950
-}
