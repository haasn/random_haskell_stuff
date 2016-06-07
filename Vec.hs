{-# LANGUAGE DataKinds, MultiParamTypeClasses, GADTs, KindSignatures
  , TypeOperators, FlexibleInstances, TypeFamilies #-}

data Nat = Z | S Nat

data Vec a (n :: Nat) where
  VNil  :: Vec a Z
  VCons :: a -> Vec a n -> Vec a (S n)

{-
class (n :: Nat) ≥ (m :: Nat)
instance (n ≥ Z)
instance (n ≥ m) => (S n ≥ S m)
-}

type family (n :: Nat) ≥ (m :: Nat) :: Bool
type instance Z   ≥ Z   = True
type instance S n ≥ Z   = True
type instance Z   ≥ S m = False
type instance S n ≥ S m = n ≥ m

data List a where
  List :: Vec a n -> List a

data ListMin a (m :: Nat) where
  ListMin :: (n ≥ m) ~ True => Vec a n -> ListMin a m

head' :: ListMin a (S Z) -> a
head' (ListMin (VCons a _)) = a
