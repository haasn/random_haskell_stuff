{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators,
             FlexibleInstances, UndecidableInstances, TypeFamilies, DataKinds,
             GADTs, FlexibleContexts, ConstraintKinds, PolyKinds #-}

data Nat = NZ | NS Nat -- Peano encoding

data Shape :: * -> Nat -> * where
    Z    :: Shape t NZ
    (:.) :: Shape t n -> t -> Shape t (NS n)
infixl 5 :.

type family Dup (x :: Nat) :: Nat where
    Dup NZ = NZ
    Dup (NS x) = NS (NS (Dup x))

dupShape :: Shape t n -> Shape t (Dup n)
dupShape Z = Z
dupShape (Z :. t) = Z :. t :. t

type family Halve (x :: Nat) :: Nat where
    Halve NZ = NZ
    Halve (NS (NS x)) = NS (Halve x)

addPairs :: Num t => Shape t n -> Shape t (Halve n)
addPairs Z = Z
addPairs (xs :. x :. y) = addPairs xs :. (x+y)
