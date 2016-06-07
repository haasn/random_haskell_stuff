{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- Keeping it simple
type Operation = [Double] -> [Double]

class Op t where
  op :: t -> Operation
  arity :: t -> Int

instance Op Double where
  op = (:)
  arity _ = 0

instance (d ~ Double, Op t) => Op (d -> t) where
  op f s = op (f x) (ys++xs) where (ys, x:xs) = splitAt (arity f - 1) s
  arity f = 1 + arity (f undefined)
