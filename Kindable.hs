{-# LANGUAGE DataKinds, PolyKinds, ScopedTypeVariables, FlexibleInstances,
    UndecidableInstances #-}

import Data.Proxy

type KindRep = String

class Kindable (t :: k) where
  kindOf :: proxy t -> KindRep

instance Kindable (a :: *) where
  kindOf _ = "*"

instance Kindable (t :: k) => Kindable (a :: Maybe k) where
  kindOf _ = "Maybe " ++ kindOf (Proxy :: Proxy t)

main = do
  putStrLn $ kindOf ""
  putStrLn $ kindOf (Proxy :: Proxy ('Just Int))
