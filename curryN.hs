{-# LANGUAGE MultiParamTypeClasses, OverlappingInstances, FlexibleInstances, GADTs, DataKinds, KindSignatures, TypeFamilies, FunctionalDependencies, UndecidableInstances, FlexibleContexts, ScopedTypeVariables #-}

import Data.Proxy

class CurryN a b x where
  curryN :: ([a] -> b) -> x

instance (b ~ r) => CurryN a b r where
  curryN f = f []

instance (a ~ c, CurryN a b x) => CurryN a b (c -> x) where
  curryN f a = curryN (f . (a:))

-- Attempt 2

{-
data Nat = Z | S Nat

type family F a b x :: Nat where
  F a b (a -> x) = S (F a b x)
  F a b b = Z

class CurryN' (n :: Nat) a b x | x n a -> b, n a b -> x where
  foo :: proxy n -> ([a] -> b) -> x

instance (b ~ r) => CurryN' Z a b r where
  foo _ f = f []

instance (a ~ c, CurryN' n a b x) => CurryN' (S n) a b (c -> x) where
  foo p f a = foo (prev p) (f . (a:))
    where prev :: proxy (S n) -> Proxy n
          prev _ = Proxy

curryN' :: forall a b x. CurryN' (F a b x) a b x => ([a] -> b) -> x
curryN' = foo (Proxy :: Proxy (F a b x))
-}
