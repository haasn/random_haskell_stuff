import Control.Lens ((&))

import Data.Foldable (toList)
import Data.List (unfoldr, foldl')
import Data.Sequence hiding (unfoldr)

select :: Ord a => [a] -> Maybe (a, [a])
select []     = Nothing
select (x:xs) =
  foldl' go (x, empty, empty) xs & \(a,x,y) -> Just (a, toList (x >< y))

 where
  go (a, x, y) b
    | b < a     = (b, x >< a <| y, empty )
    | otherwise = (a, x          , y |> b)

sort' :: Ord a => [a] -> [a]
sort' = unfoldr select
