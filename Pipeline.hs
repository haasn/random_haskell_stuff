{-# LANGUAGE OverlappingInstances, TypeFamilies, FlexibleInstances #-}

type family From a
type instance where
  From ((a -> b) -> b) = a
  From a = a

class Val a where
  val :: From a -> a

instance Val a where
  val = id

instance Val ((a -> b) -> b) where
  val = flip id
