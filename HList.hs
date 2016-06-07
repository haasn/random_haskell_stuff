{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures #-}

data HList :: [*] -> * where
  Nil  :: HList '[]
  (:*) :: a -> HList as -> HList (a ': as)
infixr 5 :*

{-
example :: HList '[Int, Char, Bool]
example = 3 (HCons 'x' (HCons True HNil))

tail :: HList (x ': xs) -> HList xs
tail (HCons _ xs) = xs
-}
