import Data.Map as M hiding (foldr)
import Data.Stream.Infinite as S

type Vertex = Int
type Weight = Int
type Edge   = (Vertex, Vertex, Weight)

relax :: Edge -> Map Vertex Weight -> Map Vertex Weight
relax (u,v,w) m | Just u' <- M.lookup u m =
  let m' = M.insert v (u' + w) m in case M.lookup v m of
    Nothing -> m'
    Just v' | u' + w < v' -> m'
    _ -> m

relax _ m = m

steps :: [Edge] -> Stream (Map Vertex Weight)
steps es = S.iterate (foldr relax `flip` es) $ M.singleton 1 0

findStable :: Eq a => Int -> Stream a -> Maybe a
findStable 0 _ = Nothing
findStable n (x :> x' :> xs)
  | x == x'   = Just x
  | otherwise = findStable (n-1) (x' :> xs)

bellman :: Int -> [Edge] -> Maybe (Map Vertex Weight)
bellman n = findStable n . steps

negativeCycle :: Int -> [Edge] -> Bool
negativeCycle n es = bellman n es == Nothing
