{-# LANGUAGE GADTs #-}

data Foo where
  Foo :: Show a => a -> Foo

foo :: String
foo = show s
  where Foo s = Foo ()
