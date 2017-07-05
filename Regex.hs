{-# LANGUAGE ViewPatterns, LambdaCase, BangPatterns #-}

import Prelude hiding (and, or)

import Control.Applicative ((<|>))
import Control.Lens

import Data.List (sort, nub, intercalate, isPrefixOf)
import Data.Maybe (catMaybes)

import Test.QuickCheck

-- DFA stuff

data DFA = DFA { alphabet :: [Char], states :: Int, start :: Int, finals :: [Int], fun :: Int -> Char -> Int }

checkD :: DFA -> String -> Bool
checkD d s = go (start d) s `elem` finals d
    where go !n []     = n
          go !n (x:xs) = go (fun d n x) xs

-- Regex stuff

data Regex = E | Lit String | Star Regex | Plus Regex | Opt Regex | Regex :* Regex | Regex :| Regex
    deriving (Eq, Show, Ord)

infixr 6 :*
infixr 5 :|

-- NOTE: still contains some bug, but I can't be bothered to find it or rework
-- this logic again

step :: Char -> Regex -> Maybe Regex
step _ E    = Nothing

step c (Lit (x:xs)) | x == c = Just (Lit xs)
step c (Lit _) = Nothing

step c (x :* y) = case step c x of
    Just E  -> Just y
    Just x' -> Just $ x' :* y
    Nothing -> if eof x then step c y else Nothing

step c (x :| y) = case (step c x, step c y) of
    (Just E,  Just E)  -> Just E
    (Just x', Just y') -> Just (x' :| y')
    (x', y')           -> x' <|> y'

step c (Star r) = case step c r of
    Nothing -> Nothing
    Just r' -> Just (Star r')

step c (Plus r) = step c (Star r)
step c (Opt  r) = step c r

eof :: Regex -> Bool
eof E    = True
eof (Lit "") = True
eof (Lit _ ) = False
eof (Star _) = True
eof (Plus r) = eof r
eof (Opt  _) = True
eof (x :* y) = eof x && eof y
eof (x :| y) = eof x || eof y

checkR :: Regex -> String -> Bool
checkR r []     = eof r
checkR r (x:xs) = case step x r of
    Just r' -> checkR r' xs
    Nothing -> False

instance Plated Regex where
    plate f E        = pure E
    plate f (Lit l)  = pure (Lit l)
    plate f (Star r) = Star <$> f r
    plate f (Plus r) = Plus <$> f r
    plate f (Opt r)  = Opt  <$> f r
    plate f (x :* y) = (:*) <$> f x <*> f y
    plate f (x :| y) = (:|) <$> f x <*> f y

or, and :: [Regex] -> Regex
or [] = E
or xs = foldr1 (:|) xs

and [] = E
and xs = foldr1 (:*) xs

-- DFA conversion and minification

regex :: DFA -> Maybe Regex
regex d@DFA { alphabet = l, fun = f } = orM $ catMaybes [α (states d) (start d) x
                                                        | x <- finals d]

    where α :: Int -> Int -> Int -> Maybe Regex
          α 0 i j = let r | i == j    = [E]
                          | otherwise = []
                    in orM $ r ++ [ Lit [a] | a <- l, f i a == j ]

          α k i j = orM $ catMaybes [ α k' i j, frg ]
             where k' = k - 1
                   frg = do f <- α k' i k
                            r <- α k' k k
                            g <- α k' k j
                            return $ f :* Star r :* g

          orM [] = Nothing
          orM rs = Just (or rs)

minify :: Regex -> Regex
minify = rewrite $ \x -> case x of
    x :| y | x == y -> Just x
    x :| y | x > y -> Just $ y :| x

    E :| r -> Just $ Opt r
    Opt E -> Just E

    Plus (Opt r) -> Just $ Star r
    Star (Opt r) -> Just $ Star r

    Opt (Plus r) -> Just $ Star r
    Opt (Star r) -> Just $ Star r

    Star E -> Just E
    Star (E :| r) -> Just $ Star r
    Plus (E :| r) -> Just $ Star r

    (a :* b :* c) :| (x :* y :* z) | [b,c] == [y,z] -> Just $ (a :| x) :* b :* c
    (a :* b :* c) :| (x :* y :* z) | [a,c] == [x,z] -> Just $ a :* (b :| y) :* c
    (a :* b :* c) :| (x :* y :* z) | [a,b] == [x,y] -> Just $ a :* b :* (c :| z)

{-
    -- These make the output larger
    (a :| b) :* r -> Just $ a :* r :| b :* r
    r :* (a :| b) -> Just $ r :* a :| r :* b
-}

    a :| Star b :* c | a == c -> Just $ Star b :* c
    a :| c :* Star b | a == c -> Just $ c :* Star b

    E :* r -> Just r
    r :* E -> Just r

    Lit a :* Lit b -> Just $ Lit (a++b)

    a :* Star b :* c | b == minify (a :* c) && a == c -> Just $ Plus b

    x :* Star y | x == y -> Just $ Plus x
    Star x :* y | x == y -> Just $ Plus x

    Star x :* Plus y | x == y -> Just $ Star x
    Star x :* Opt  y | x == y -> Just $ Star x
    Plus x :* Star y | x == y -> Just $ Star y
    Opt  x :* Star y | x == y -> Just $ Star y

    Star x :| Plus y | x == y -> Just $ Star x
    Star x :| Opt  y | x == y -> Just $ Star x
    Plus x :| Star y | x == y -> Just $ Star y
    Opt  x :| Star y | x == y -> Just $ Star y

    (x :* y) :* z -> Just $ x :* (y :* z)
    (x :| y) :| z -> Just $ x :| (y :| z)

    _ -> Nothing

-- Testing

propMinify :: Regex -> String -> Bool
propMinify r s = checkR r s == checkR (minify r) s

propDFA :: DFA -> String -> Bool
propDFA d s = case regex d of
    Nothing -> checkD d s == False
    Just  r -> checkD d s == checkR r s

-- Pretty-printing and examples

pretty :: Int -> Regex -> String
pretty _ E        = "ε"
pretty n (Lit l)  = parens (length l > 1 && n > 1) l
pretty n (Star r) = parens (n > 2) $ pretty 2 r ++ "*"
pretty n (Plus r) = parens (n > 2) $ pretty 2 r ++ "+"
pretty n (Opt r)  = parens (n > 2) $ pretty 2 r ++ "?"
pretty n (x :* y) = parens (n > 1) $ pretty 1 x ++ pretty 1 y
pretty n (x :| y) = parens (n > 0) $ pretty 0 x ++ "|" ++ pretty 0 y

parens :: Bool -> String -> String
parens True  s = "(" ++ s ++ ")"
parens False s = s

goal :: DFA
goal = DFA { alphabet = "01", states = 4, start = 1, finals = [1], fun = f }
    where f n '0' = case n of 1 -> 2; 2 -> 1; 3 -> 4; 4 -> 3
          f n '1' = case n of 1 -> 3; 3 -> 1; 2 -> 4; 4 -> 2

test :: DFA
test = DFA { alphabet = "01", states = 2, start = 1, finals = [2], fun = f }
    where f 1 _ = 2
          f 2 _ = 1
