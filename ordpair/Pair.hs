{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, TypeOperators
  , TemplateHaskell, GADTs #-}

import GHC.Exts (Constraint)
import Data.Singletons

singletons [d| data Nat = Zero | Succ Nat |]

type family (n :: Nat) <= (m :: Nat) :: Constraint
type instance Zero   <=      m = ()
type instance Succ n <= Succ m = n <= m

data Pair where
  Pair :: (n <= m) => SNat n -> SNat m -> Pair
