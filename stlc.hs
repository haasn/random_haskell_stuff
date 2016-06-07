{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances #-}

import Prelude hiding (id, const)

import Data.Singletons
import Data.Singletons.TypeLits
import GHC.TypeLits

data Type a = Type a :→ Type a | Prim a
infixr 5 :→

data Lam (env :: [Type k]) (t :: Type k) where
  -- de bruijn
  Var0 :: Lam (t ': env) t
  VarS :: Lam env t -> Lam (s ': env) t

  App :: Lam env (a :→ b) -> Lam env a -> Lam env b
  Lam :: Lam (a ': env) b -> Lam env (a :→ b)

-- Example

data U   = A | B
type Env = '[ Prim A, Prim B ]

a :: Lam Env (Prim A)
b :: Lam Env (Prim B)
a = Var0
b = VarS Var0

id :: Lam e (t :→ t)
id = Lam Var0

const :: Lam e (a :→ b :→ a)
const = Lam (Lam (VarS Var0))

program :: Lam Env (Prim B)
program = App (App (App const id) a) b
