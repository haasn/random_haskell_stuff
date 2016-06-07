{-# LANGUAGE DeriveGeneric, BangPatterns, DeriveFunctor, LambdaCase #-}

import Control.Applicative
import Control.DeepSeq (NFData, force)
import Data.Functor.Identity
import GHC.Generics (Generic)

import Test.QuickCheck
import Criterion
import qualified Criterion.Main

data Tree a = Empty | Bin !(Tree a) a !(Tree a)
    deriving (Eq, Show, Generic, Functor)

instance CoArbitrary a => CoArbitrary (Tree a)
instance Arbitrary   a => Arbitrary   (Tree a) where
    arbitrary = oneof [pure Empty, liftA3 Bin arbitrary arbitrary arbitrary]
    shrink    = genericShrink

instance NFData a => NFData (Tree a)

enumB1 :: Tree a -> [a]
enumB1 = concat . go
    where go Empty       = []
          go (Bin l x r) = [x] : merge (go l) (go r)

          merge (x:xs) (y:ys) = (x++y) : merge xs ys
          merge xs     []     = xs
          merge []     ys     = ys

enumB2 :: Tree a -> [a]
enumB2 t = go [t]
    where go []               = []
          go (Empty     : ts) = go ts
          go (Bin l x r : ts) = x : go (ts++[l,r])

enumB3 :: Tree a -> [a]
enumB3 t = go [] [t]
    where go [] [] = []
          go rs [] = go [] (reverse rs)
          go rs (t:ts) = case t of
                Empty     -> go rs ts
                Bin l x r -> x : go (r:l:rs) ts

-- Generalized to arbitrary fold

foldB :: Monoid a => Tree a -> a
foldB t = go [] [t]
    where go [] [] = mempty
          go rs [] = go [] (reverse rs)
          go rs (t:ts) = case t of
                Empty     -> go rs ts
                Bin l x r -> x `mappend` go (r:l:rs) ts

enumB4 :: Tree a -> [a]
enumB4 = foldB . fmap pure

-- Generalized to arbitrary traversal

-- 'weak' traversal (like fold)
wtraverseB :: Applicative f => (a -> f b) -> Tree a -> f [b]
wtraverseB f t = go [] [t]
    where go [] [] = pure []
          go rs [] = go [] (reverse rs)
          go rs (t:ts) = case t of
                Empty     -> go rs ts
                Bin l x r -> (:) <$> f x <*> go (r:l:rs) ts

enumB5 :: Tree a -> [a]
enumB5 = getConst . wtraverseB (\x -> Const [x])

-- strong traversal
traverseB :: Monad f => (a -> f b) -> Tree a -> f (Tree b)
traverseB f t = go [] [pure t]
    where go [] [] = pure Empty
          go rs [] = go [] (reverse rs)
          go rs (t:ts) = case fst t of
                Empty     -> go rs ts
                Bin l x r -> c <$> f x 

{-
traverseB :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseB f Empty = pure Empty go [] [t]
    where go [] [] = pure (Empty, Empty)
          go rs [] = go [] (reverse rs)
          go rs (t:ts) = case t of
                Empty     -> go rs ts
                Bin l x r -> c <$> f x <*> go (r:l:rs) ts

          c x (l,r) = Bin x l r

traverseB :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseB f t = go t
    where go Empty       = pure Empty
          go (Bin l x r) = [x] : merge (go l) (go r)

          merge (x:xs) (y:ys) = (x++y) : merge xs ys
          merge xs     []     = xs
          merge []     ys     = ys

enumB6 :: Tree a -> [a]
enumB6 = getConst . traverseB (\x -> Const [x])
-}

-- Test
funs  = [enumB1, enumB2, enumB3, enumB4, enumB5]
props = map (\f x -> enumB3 x == f (x :: Tree Int)) funs
     ++ [ \x -> Identity (enumB3 x :: [Int]) == wtraverseB Identity x ]

-- Benchmark
benchB = [ bench ("enumB"++show i) $ whnf (length . f) tree
         | (i,f) <- zip [1..] funs]
    where !tree = force $ iterate (\t -> Bin t () t) Empty !! 8

main = do
    mapM_ quickCheck props
    Criterion.Main.defaultMain benchB
