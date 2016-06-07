{-# LANGUAGE TupleSections, ViewPatterns, TemplateHaskell #-}

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Ord as Ord

import Prelude hiding (max, succ)

-- Types

data Val = Val Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Show, Eq)
data Card = Card { _val :: Val, _suit :: Suit} deriving (Show, Eq)
makeLenses ''Card

instance Ord.Ord Card where
  compare = Ord.comparing _val

type Hand  = [Card]
type Score = Maybe Int

-- Reading: ugly style

readV :: Char -> Val
readV 'J' = Jack
readV 'Q' = Queen
readV 'K' = King
readV 'A' = Ace
readV 'T' = Val 10
readV  n  = Val $ read [n]

readS :: Char -> Suit
readS 'H' = Hearts
readS 'C' = Clubs
readS 'S' = Spades
readS 'D' = Diamonds

readC :: String -> Card
readC [v,s] = Card (readV v) (readS s)

readH :: String -> Hand
readH = map readC . words

-- Utility functions

succ :: Val -> Val
succ (Val 10) = Jack
succ (Val n) = Val (n+1)
succ Jack    = Queen
succ Queen   = King
succ King    = Ace

value :: Val -> Int
value (Val i) = i
value Jack    = 11
value Queen   = 12
value King    = 13
value Ace     = 14

-- Misc helpers

max :: [Card] -> Maybe Card
max = maximumOf folded

score :: Functor f => a -> f b -> f a
score = fmap . const

npairs :: Int -> Hand -> [Card]
npairs n h = [x | (x:xs) <- subsequences h
                , length xs == n-1
                , all (\c -> x^.val == c^.val) xs]

-- Comparing hands

compareH :: Hand -> Hand -> Ordering
compareH x y = case Ord.comparing result x y of
  EQ -> (Ord.compare `on` reverse.sort) x y
  c  -> c
  where result h = maximum $ catMaybes (rules ?? h)
        rules = [ highCard, onePair, twoPairs, threeOfAKind, straight, flush
                , fullHouse, fourOfAKind, straightFlush, royalFlush]

-- Rules

highCard :: Hand -> Score
highCard _ = Just 1

onePair :: Hand -> Score
onePair = score 2 . max . npairs 2

twoPairs :: Hand -> Score
twoPairs h | length p >= 2 = score 2 (max p)
           | otherwise     = Nothing
  where p = npairs 2 h

threeOfAKind :: Hand -> Score
threeOfAKind = score 3 . max . npairs 3

straight :: Hand -> Score
straight (sort -> h) = do
  guard . and $ zipWith (==) (tail h) (h & each.val %~ succ)
  score 4 (max h)

flush :: Hand -> Score
flush h@(x:xs) = guard (all (==x) xs) >> score 5 (max h)

fullHouse :: Hand -> Score
fullHouse h = do
  p <- max (npairs 2 h)
  max . npairs 3 . filter (/=p) $ h
  return (6)

fourOfAKind :: Hand -> Score
fourOfAKind = score 7 . max . npairs 4

straightFlush :: Hand -> Score
straightFlush h = 8 <$ straight h <* flush h

royalFlush :: Hand -> Score
royalFlush (sort -> h) = do
  flush h
  guard $ map _val h == [Val 10, Jack, Queen, King, Ace]
  return 9
