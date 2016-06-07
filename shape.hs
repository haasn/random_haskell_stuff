{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators,
             FlexibleInstances, UndecidableInstances, TypeFamilies, DataKinds,
             GADTs, FlexibleContexts, ConstraintKinds, PolyKinds #-}

import GHC.Exts (Constraint)

data Z = Z deriving Show
data tail :. head = tail :. head deriving Show
infixl 5 :.

-- Well this is the "dumb" way of doing it

class DupC x y | x -> y, y -> x where
    dup :: x -> y

instance DupC Z Z where
    dup Z = Z

instance DupC t t' => DupC (t :. h) (t' :. h :. h) where
    dup (t :. h) = dup t :. h :. h

-- Different approach:

-- Instead of working with “dumb” Z/:. values directly, we instead work
-- with a singleton that embeds the type rigorously
data SShape :: [*] -> * where
    SZ    :: SShape '[]
    (:%.) :: SShape tail -> head -> SShape (head ': tail)
infixl 5 :%.

-- Our type-action is now an injective, closed type familiy
type family DupF (t :: [*]) where
    DupF '[] = '[]
    DupF (x ': xs) = x ': x ': DupF xs

dupShape :: SShape t -> SShape (DupF t)
dupShape SZ = SZ
dupShape (t :%. h) = dupShape t :%. h :%. h

-- For backwards compatibility with the pre-DataKinds shapes, we can
-- create projection and reflection classes

class Shape (x :: [*]) (y :: *) | x -> y, y -> x where
    toShape :: y -> SShape x
    fromShape :: SShape x -> y

instance Shape '[] Z where
    toShape Z = SZ
    fromShape SZ = Z

instance Shape ts ys => Shape (t ': ts) (ys :. t) where
    toShape (t :. h) = toShape t :%. h
    fromShape (t :%. h) = fromShape t :. h

oldDupShape :: (Shape a b, Shape (DupF a) b') => b -> b'
oldDupShape = fromShape . dupShape . toShape

-- Add pairs

type family All (c :: k -> Constraint) (ts :: [k]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

type family ZipPairs (t :: [*]) :: [*] where
    ZipPairs '[]            = '[]
    ZipPairs (x ': x ': xs) = x ': xs

type family ZipC (ts :: [*]) :: Constraint where
    ZipC '[] = ()
    ZipC (x ': y ': xs) = (x ~ y, ZipC xs)

zipPairs :: (All Num t, ZipC t) => SShape t -> SShape (ZipPairs t)
zipPairs SZ = SZ
zipPairs (t :%. x :%. x') = t :%. x+x'
