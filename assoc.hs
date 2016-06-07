{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, RankNTypes #-}

import Control.Applicative
import Control.Lens

newtype Assoc k a = Assoc { runAssoc :: [(k,a)] }

instance Wrapped [(k,a)] [(k',a')] (Assoc k a) (Assoc k' a') where
  wrapped = iso Assoc runAssoc

-- Better name?
assocKey :: Eq k => k -> IndexedTraversal' k [(k,a)] a
assocKey _ _ [] = pure []
assocKey i f (x@(k,v):xs) = x' <*> assocKey i f xs
  where x' | i == k    = cons . (,) k <$> indexed f k v
           | otherwise = pure (x:)

type instance Index   (Assoc k a) = k
type instance IxValue (Assoc k a) = a

instance (Eq k, Applicative f) => Ixed f (Assoc k a) where
  ix i f (Assoc xs) = Assoc <$> assocKey i f xs

instance Traversable (Assoc k) where
  traverse = traverse._2

instance TraversableWithIndex (Assoc k) where
  


assoc :: Iso' [(k,a)] (Assoc k a)
assoc = wrapping Assoc

{- Example:


-}
