{-# LANGUAGE RankNTypes, TypeOperators #-}

class Isomorphic k where
  iso :: (a -> b) -> (b -> a) -> k a b

instance Isomorphic (->) where
  iso = const

data Iso a b = Iso (a -> b) (b -> a)

instance Isomorphic Iso where
  iso = Iso

type a <-> b = forall k. Isomorphic k => k a b

from :: Iso a b -> b <-> a
from (Iso f g) = iso g f

data Foo = Foo
data Bar = Bar

foo :: Foo <-> Bar
foo = iso (\Foo -> Bar) (\Bar -> Foo)

example1 :: Bar
example1 = foo Foo

example2 :: Foo
example2 = from foo Bar
