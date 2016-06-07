data TL a = TL [a] | Cons [a] (TL a) | Snoc (TL a) [a]

unTL :: TL a -> [a]
unTL (TL xs)      = xs
unTL (Cons xs tl) = xs ++ unTL tl
