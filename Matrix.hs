{-# LANGUAGE DataKinds, GADTs #-}

data Nat = Z | S (Nat)

data Vec (n :: Nat) a where
  VNil  :: Vec Z
  VCons :: a -> Vec n -> Vec (S n)

type Matrix n m a = Vec n (Vec m a)

mtranspose :: Matrix n m a -> Matrix m n a
mtranspose VNil = VNil
mtranspose
