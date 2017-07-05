{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses,
    FunctionalDependencies, RankNTypes, MagicHash, FlexibleInstances,
    UndecidableInstances, FlexibleContexts, ScopedTypeVariables #-}

import GHC.Prim (Proxy#, proxy#)

type family IsFlippedFmap a b :: Bool where
    IsFlippedFmap (_ -> _) _ = False
    IsFlippedFmap _ _ = True

class MagicFmap (flipped :: Bool) a b c | flipped a b -> c where
    magicFmapImpl :: Proxy# flipped -> a -> b -> c

instance (Functor f, a ~ (x -> y), b ~ f x, c ~ f y) => MagicFmap False a b c where
    magicFmapImpl _ = fmap

instance (Functor f, a ~ (x -> y), b ~ f x, c ~ f y) => MagicFmap True b a c where
    magicFmapImpl _ = flip fmap

magicFmap :: forall arg1 arg2 argRes. MagicFmap (IsFlippedFmap arg1 arg2) arg1 arg2 argRes => arg1 -> arg2 -> argRes
magicFmap = magicFmapImpl (proxy# :: Proxy# (IsFlippedFmap arg1 arg2))

{-

λ :t magicFmap (+1)
magicFmap (+1) :: (Num x, Functor f) => f x -> f x

λ :t magicFmap [1..10]
magicFmap [1..10] :: (Enum x, Num x) => (x -> y) -> [y]

-}
