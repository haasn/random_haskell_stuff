data Tree a = Empty | Node
  { node  :: a
  , left  :: Tree a
  , right :: Tree a

  -- Insertion optimization
  , size' :: Int
  }
  deriving (Eq, Show)

size :: Tree a -> Int
size Empty = 0
size n     = size' n

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty          = Node x Empty Empty 1
insert x (Node y l r n)
  -- Insert into the smaller sub-tree
  | size l > size r = Node a l (insert b r) (n+1)
  | otherwise       = Node a (insert b l) r (n+1)
  where a = min x y; b = max x y

merge :: Ord a => Tree a -> Tree a -> Tree a
merge x     Empty = x
merge Empty y     = y
merge xn@(Node x lx rx n1) yn@(Node y ly ry n2)
  | x <= y    = Node x (merge lx rx) yn (n1+n2)
  | otherwise = Node y xn (merge ly ry) (n1+n2)

heapify :: Ord a => [a] -> Tree a
heapify = foldr insert Empty

unheap :: Ord a => Tree a -> [a]
unheap Empty          = []
unheap (Node x l r _) = x : unheap (merge l r)

heapsort :: Ord a => [a] -> [a]
heapsort = unheap . heapify
