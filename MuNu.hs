{-# LANGUAGE Rank2Types, ExistentialQuantification, LambdaCase #-}

newtype Mu f  = Mu (forall r. (f r -> r) -> r)
data    Nu f  = forall a. Nu a (a -> f a)

outNu :: Functor f => Nu f -> f (Nu f)
outNu (Nu x f) = fmap (\x' -> Nu x' f) (f x)

inNu :: Functor f => f (Nu f) -> Nu f
inNu f = Nu f (fmap outNu)

outMu :: Functor f => Mu f -> f (Mu f)
outMu (Mu f) = f (fmap inMu)

inMu :: Functor f => f (Mu f) -> Mu f
inMu f = Mu _ _
