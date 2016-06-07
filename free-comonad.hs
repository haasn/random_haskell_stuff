import Control.Comonad
import Control.Monad.Identity

data Cofree f a = Cofree a (f (Cofree f a))

instance Functor f => Functor (Cofree f) where
  fmap f (Cofree a o) = Cofree (f a) (fmap (fmap f) o)

instance Functor f => Comonad (Cofree f) where
  extract (Cofree a _)     = a
  duplicate c@(Cofree a o) = Cofree c (fmap duplicate o)

-- Rose trees

type Tree = Cofree []

example1 :: Tree Int
example1 = Cofree 3 [Cofree 4 [], Cofree 5 [Cofree 6 []]]

-- Nonempty lists

type NonEmpty = Cofree Maybe

example2 :: NonEmpty Char
example2 = Cofree 'a' (Just (Cofree 'b' (Just (Cofree 'c' Nothing))))

-- Infinite streams

type Stream = Cofree Identity

example3 :: Stream Bool
example3 = Cofree True (Identity (Cofree False (Identity example3)))
