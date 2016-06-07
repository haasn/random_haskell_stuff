{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, LiberalTypeSynonyms, GADTs
           , ScopedTypeVariables, TypeOperators, FlexibleContexts
           , UndecidableInstances, PolyKinds #-}

import Data.Char (chr)
import GHC.TypeLits

-- Hand-rolled natural numbers on the type level, plus singletons

data N = Z | S N deriving Show

nelim :: a -> (a -> a) -> N -> a
nelim z _ Z = z
nelim z s (S n) = s (nelim z s n)

data instance Sing (n :: N) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)

instance SingI Z where
  sing = SZ

instance SingI n => SingI (S n) where
  sing = SS sing

instance SingE (KindParam :: OfKind N) where
  type DemoteRep (KindParam :: OfKind N) = N
  fromSing SZ = Z
  fromSing (SS n) = S (fromSing n)

-- Type level literals via binary aliases

type Twice f x = f (f x)

type N1   x = S x
type N2   x = Twice N1  x
type N4   x = Twice N2  x
type N8   x = Twice N4  x
type N16  x = Twice N8  x
type N32  x = Twice N16 x
type N64  x = Twice N32 x
type N128 x = Twice N64 x

-- Type-level strings as lists of nat-encoded Unicode codepoints

data instance Sing (xs :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

instance SingI ('[] :: [k]) where
  sing = SNil

instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

instance SingE (KindParam :: OfKind a) => SingE (KindParam :: OfKind [a]) where
  type DemoteRep (KindParam :: OfKind [a]) = [DemoteRep (KindParam :: OfKind a)]
  fromSing SNil = []
  fromSing (SCons x xs) = fromSing x : fromSing xs

fromNat :: N -> Int
fromNat = nelim 0 succ

fromNatStr :: [N] -> String
fromNatStr = map (chr . fromNat)

printStatic :: forall proxy (n :: [N]). SingRep n => proxy n -> IO ()
printStatic _ = putStrLn . fromNatStr $ fromSing (sing :: Sing n)

-- Hello world as Unicode codepoints

type HelloWorld =
  '[ N64 (N8 Z)                       -- H
   , N64 (N32 (N4 (N1 Z)))            -- e
   , N64 (N32 (N8 (N4 Z)))            -- l
   , N64 (N32 (N8 (N4 Z)))            -- l
   , N64 (N32 (N8 (N4 (N2 (N1 Z)))))  -- o
   , N32 (N8 (N4 Z))                  -- ,
   , N32 Z
   , N64 (N32 (N16 (N4 (N2 (N1 Z))))) -- w
   , N64 (N32 (N8 (N4 (N2 (N1 Z)))))  -- o
   , N64 (N32 (N16 (N2 Z)))           -- r
   , N64 (N32 (N8 (N4 Z)))            -- l
   , N64 (N32 (N4 Z))                 -- d
   , N32 (N1 Z)                       -- !
   ]

main :: IO ()
main = printStatic (sing :: Sing HelloWorld)
