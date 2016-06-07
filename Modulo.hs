{-# LANGUAGE DataKinds, ScopedTypeVariables, PolyKinds #-}

import GHC.TypeLits
import Data.Singletons

newtype Mod (m :: Nat) a = Mod { unMod :: a }

-- FIXME
liftMod :: forall m a. (SingRep m, Integral a) => (a -> a) -> Mod m a -> Mod m a
liftMod f (Mod a) = Mod $ f a `mod` fromSing (sing :: Sing m)

liftMod2 :: forall m a. (SingRep m, Integral a) => (a -> a -> a) -> Mod m a -> Mod m a -> Mod m a
liftMod2 f (Mod a) (Mod b) = Mod $ f a b `mod` fromSing (sing :: Sing m)

instance (SingRep m, Integral a) => Num (Mod m a) where
  (+) = liftMod2 (+)
  (-) = liftMod2 (-)
  (*) = liftMod2 (*)

  negate = liftMod negate
  abs    = liftMod abs
  signum = liftMod signum

  fromInteger n = Mod $ fromInteger n `mod` fromSing (sing :: Sing m)

withMod :: p m -> Mod m a -> a
withMod _ = unMod
