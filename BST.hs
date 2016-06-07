data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert a Leaf = Node a Leaf Leaf
insert a (Node a' l r)
  | a < a'    = Node a' (insert a l) r
  | otherwise = Node a' l (insert a r)
  -- This is arbitrarily right-biased for the equal case.
  -- An alternative approach could be to add an extra ‘size’ field
  -- and then insert to the one with the smaller size.

contains :: Ord a => a -> Tree a -> Bool
contains _ Leaf = False
contains a (Node a' l r) = case compare a a' of
  EQ -> True
  LT -> contains a l
  GT -> contains a r
