{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, GADTs, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds #-}

import GHC.Prim (Constraint)

--- does the type family version have to be so verbose
--- compared with the fundep version?

-- *  parts of hlist needed
data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)

instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (HCons x l) = let 'H':'[':s = show l
               in "H[" ++ show x ++ 
                      (if s == "]" then s else ", " ++ s)

infixr 2 `HCons`

type family Zip (xs :: [*]) (ys :: [*]) :: [*]
type instance Zip '[] y = '[]
type instance Zip x '[] = '[]
type instance Zip (x ': xs) (y ': ys) = (x,y) ': Zip xs ys

hzip :: HList x -> HList y -> HList (Zip x y)
hzip HNil _ = HNil
hzip _ HNil = HNil
hzip (HCons x xs) (HCons y ys) = HCons (x,y) (hzip xs ys)


type family UnZip (xs :: [*]) :: ([*], [*])
type instance UnZip '[] = '( '[], '[])
type instance UnZip ((x,y) ': xs) = UnZip' x y (UnZip xs)

type family UnZip' (x :: *) (y :: *) (xs :: ([*], [*])) :: ([*], [*])
type instance UnZip' x y '(xs,ys) = '(x ': xs, y ': ys)

type family UnZippable (xs :: [*]) :: Constraint
type instance UnZippable '[] = ()
type instance UnZippable ((u,v) ': xs) = UnZippable xs

hunzip :: ('(xs,ys) ~ UnZip x, UnZippable x) => HList x -> (HList xs, HList ys)
hunzip HNil = (HNil, HNil)
hunzip (HCons (x,y) z) = (HCons x xs, HCons y ys) where (xs,ys) = hunzip z
