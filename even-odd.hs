{-# LANGUAGE DataKinds, GADTs #-}

data Parity = Even | Odd

data Nat (p :: Parity) where
  Z  :: Nat Even
  SO :: Nat Even -> Nat Odd
  SE :: Nat Odd  -> Nat Even

halve :: Nat Even -> Nat p
