{-# LANGUAGE GADTs #-}

class Op t where
  op :: t -> [Double] -> [Double]

instance Op Double where
  op = (:)

instance (d ~ Double, Op t) => Op (d -> t) where
  op _ []     = error "Empty stack"
  op f (x:xs) = op (f x) xs
