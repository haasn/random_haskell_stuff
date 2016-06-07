{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

import GHC.TypeLits
import Data.Singletons

type family Fib (n :: Nat) :: Nat where
  Fib 0 = 0
  Fib 1 = 1
  Fib n = Fib (n-1) + Fib (n-2)

main = print $ fromSing (sing :: Sing (Fib 20))
