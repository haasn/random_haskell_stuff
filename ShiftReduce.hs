parse :: ([token] -> Maybe token -> Maybe [token]) -> [token] -> [token]
parse table = go []
 where
  go ts []     = reduce (`table` Nothing) ts
  go ts (x:xs) = case table ts (Just x) of
    Just ts' -> go ts' (x:xs)
    Nothing  -> go (x:ts) xs

reduce :: (a -> Maybe a) -> a -> a
reduce f a = maybe a (reduce f) (f a)
