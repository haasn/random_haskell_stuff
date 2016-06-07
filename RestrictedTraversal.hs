import Control.Applicative (Applicative, pure)
import Control.Lens

restrict :: Applicative f => (a -> Bool) -> LensLike' f s a -> LensLike' f s a
restrict c t f = t f'
  where f' a = if c a then f a else pure a

{- Examples:

GHCi> restrict (>5) traverse %~ negate $ [1..10]
[1,2,3,4,5,-6,-7,-8,-9,-10]

GHCi> zipper [[1],[2,3,4],[6,8,9]] % fromWithin (traverse.traverse % restrict even) % rightmost % focus .~ 100 % rezip
[[1],[2,3,4],[6,100,9]]

-}
