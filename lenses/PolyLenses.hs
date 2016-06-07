{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

import Control.Arrow (first)
import Control.Comonad.Store
import Control.Monad (join)
import Control.Monad.Identity
import Control.Monad.State

-- ‘Monomorphic’ lenses, named in terms of the package that uses them

data    FCL    a b = FCL (a -> b) (b -> a -> a)
type    Lens   a b = forall f. Functor f => (b -> f b) -> a -> f a
newtype DA     a b = DA (b -> a -> (b, a))
newtype Lenses a b = Lenses (forall x. State b x -> State a x)
newtype DL     a b = DL (a -> Store b a)
data    IsoL   a b = forall c. IsoL (a -> (b, c)) ((b, c) -> a)

-- Modify functions for the above

modify1 :: FCL a b -> (b -> b) -> a -> a
modify1 (FCL get put) f = join (put . f . get)

modify2 :: Lens a b -> (b -> b) -> a -> a
modify2 l f = runIdentity . l (Identity . f)

modify3 :: DA a b -> (b -> b) -> a -> a
modify3 (DA l) f = snd . join (l . f . fst . l undefined)

modify4 :: Lenses a b -> (b -> b) -> a -> a
modify4 (Lenses l) = execState . l . modify

modify5 :: DL a b -> (b -> b) -> a -> a
modify5 (DL l) f x = let (a, b) = runStore (l x) in a (f b)

modify6 :: IsoL a b -> (b -> b) -> a -> a
modify6 (IsoL get put) f = put . first f . get

-- Versions of the lenses that allow polymorphic updates

data    FCL'    a a' b b' = FCL' (a -> b) (b' -> a -> a')
type    Lens'   a a' b b' = forall f. Functor f => (b -> f b') -> a -> f a'
newtype DA'     a a' b b' = DA' (b' -> a -> (b, a'))
newtype Lenses' a a' b b' = Lenses' (forall x. State' b b' x -> State' a a' x)
newtype DL'     a a' b b' = DL' (a -> Store' b b' a')
data    IsoL'   a a' b b' = forall c. IsoL' (a -> (b, c)) ((b', c) -> a')

-- Polymorphic State monad needed for Lenses'

data State' s s' a = State' { runState' :: s -> (a, s') }

execState' :: State' s s' a -> s -> s'
execState' s = snd . runState' s

modify' :: (s -> s') -> State' s s' ()
modify' f = State' ((,) () . f)

-- Polymorphic Store comonad needed for DL'

newtype Store' b b' a' = Store' { runStore' :: (b' -> a', b) }

-- Modify functions with polymorphic updates.
-- Remarkably, the body of every single function is basically unchanged

modify1' :: FCL' a a' b b' -> (b -> b') -> a -> a'
modify1' (FCL' get put) f = join (put . f . get)

modify2' :: Lens' a a' b b' -> (b -> b') -> a -> a'
modify2' l f = runIdentity . l (Identity . f)

modify3' :: DA' a a' b b' -> (b -> b') -> a -> a'
modify3' (DA' l) f = snd . join (l . f . fst . l undefined)

modify4' :: Lenses' a a' b b' -> (b -> b') -> a -> a'
modify4' (Lenses' l) = execState' . l . modify'

modify5' :: DL' a a' b b' -> (b -> b') -> a -> a'
modify5' (DL' l) f x = let (a, b) = runStore' (l x) in a (f b)

modify6' :: IsoL' a a' b b' -> (b -> b') -> a -> a'
modify6' (IsoL' get put) f = put . first f . get
