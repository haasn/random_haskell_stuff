{-# LANGUAGE ViewPatterns #-}

import Control.Lens
import Control.Parallel (par)
import GHC.Conc (numCapabilities)

import Data.Foldable (Foldable)
import Data.List (foldl')
import Data.List.Split.Lens
import Data.Monoid

data Tree a = Empty | Tip a | Branch (Tree a) (Tree a)

parFold :: Monoid a => [a] -> a
parFold = fold . build
 where
  build :: [a] -> Tree a
  build [ ] = Empty
  build [a] = Tip a
  build xs  = let (x,y) = splitAt (length xs `div` 2) xs
              in Branch (build x) (build y)

  fold :: Monoid a => Tree a -> a
  fold Empty   = mempty
  fold (Tip a) = a
  fold (Branch l r) = let (x,y) = (fold l, fold r)
                      in x `par` y `par` x <> y

parFold' :: Monoid a => [a] -> a
parFold' [ ] = mempty
parFold' [a] = a
parFold' xs  = y `par` x <> y
  where (parFold' -> x, parFold' -> y) = splitAt (length xs `div` 2) xs


parFold'' :: Monoid a => [a] -> a
parFold'' xs = xs^.chunking (length xs `div` numCapabilities) folded.to (foldl' mappend mempty)

main = print . getSum . foldl' mappend mempty . map Sum $ [1..10000000]
