data Free f a = Free (f (Free f a)) | Pure a

cata :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
cata z f (Pure a) = z a
cata z f (Free g) = f $ fmap (cata z f) g
