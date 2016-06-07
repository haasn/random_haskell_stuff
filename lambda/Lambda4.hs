import Control.Monad (liftM2)
import Data.Void

data N n = S n | Z
  deriving (Show, Eq, Functor)

data TermF v
  = Unit
  | Var v
  | App (TermF v) (TermF v)
  | Abs (TermF (N v))
  deriving (Show, Eq, Functor)

instance Monad TermF where
  return = Var

  Unit    >>= _ = Unit
  Var v   >>= f = f v
  App a b >>= f = App (a >>= f) (b >>= f)
  Abs x   >>= f = Abs $ x >>= g
    where
      g Z     = return Z
      g (S x) = fmap S (f x)

-- No free variables by design
type Lambda = TermF Void

s, k, i :: Lambda
s = Abs (Abs (Abs (App (App (Var (S (S Z))) (Var Z)) (App (Var (S Z)) (Var Z)))))
k = Abs (Abs (Var (S Z)))
i = Abs (Var Z)
