{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances, TypeFamilies, UndecidableInstances #-}

import Data.Singletons
import Data.Singletons.TypeLits
import GHC.TypeLits

{-
data Vec (n :: Nat) a where
  VNil  :: Vec 0 a
  VCons :: a -> Vec n a -> Vec (n+1) a
-}

data MonoT a where
  VarT :: a -> MonoT a
  AppT :: a -> [MonoT a] -> MonoT a

data PolyT a where
  MonoT   :: MonoT a -> PolyT a
  ForAllT :: a -> PolyT a -> PolyT a


type family FreeM (t :: MonoT a) :: [a] where
  FreeM (VarT     t) = '[t]

  FreeM (AppT c '[])       = '[]
  FreeM (AppT c (t ': ts)) = Merge (FreeM t) (FreeM (AppT c ts))

type family Free (t :: PolyT a) :: [a] where
  Free ('MonoT    t) = FreeM t
  Free (ForAllT v t) = Remove v (Free t)

-- List functions

type family Merge (a :: [k]) (b :: [k]) :: [k] where
  Merge '[] xs = xs
  Merge xs '[] = xs
  Merge (x ': xs) (y ': ys) = Merge' (x <=? y) x y xs ys

type family Merge' c x y xs ys where
  Merge' True  x y xs ys = x ': Merge xs (y ': ys)
  Merge' False x y xs ys = y ': Merge (x ': xs) ys

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove x '[]       = '[]
  Remove x (x ': xs) = xs
  Remove x (y ': xs) = y ': Remove x xs
