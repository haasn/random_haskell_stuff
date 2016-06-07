{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs, KindSignatures #-}

class Cond b a where
    cond :: b -> a -> a -> a

instance Cond Bool a where
    cond b x y = if b then x else y

data Exp :: * -> * where
    Lit  :: t -> Exp t
    Cond :: Exp Bool -> Exp t -> Exp t -> Exp t

instance a ~ Exp t => Cond (Exp Bool) a where
    cond = Cond
