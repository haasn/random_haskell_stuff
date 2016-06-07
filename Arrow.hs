{-# LANGUAGE TupleSections #-}

import Control.Category

class Category a => Arrow a where
  foo :: a b (c -> d) -> a b c -> a b d
  bar :: c -> a b c

arr :: Arrow a => (b -> c) -> a b c
arr f = foo (bar f) Control.Category.id

first :: Arrow a => a b c -> a (b, d) (c, d)
first f = foo (arr (\(_,d) -> (,d))) (arr fst >>> f)
