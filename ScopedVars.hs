{-# LANGUAGE ScopedTypeVariables #-}

data Proxy k = Proxy

class Foo a where
  foo :: p a -> String

instance Foo Int where
  foo _ = "one"

instance Foo Char where
  foo _ = "two"

-- Works
bar1 :: Foo a => p a -> String
bar1 = foo

-- Does not
bar2 :: forall a p. Foo a => p a -> String
bar2 _ = foo (Proxy :: Proxy a)
