{-# LANGUAGE ViewPatterns, LambdaCase #-}

import Prelude hiding (and, or)
import Data.List (sort, nub, intercalate, isPrefixOf)
import Control.Lens

data DFA = DFA { alphabet :: [Char], states :: Int, start :: Int, finals :: [Int], fun :: Int -> Char -> Int }

data Regex = E | Lit String | Star Regex | Plus Regex | Opt Regex | Regex :* Regex | Regex :| Regex
    deriving (Eq, Show, Ord)

infixr 6 :*
infixr 5 :|

match :: Regex -> String -> [String]
match E s = [s]
match (Lit x) s | x `isPrefixOf` s = [drop (length x) s]
match (Star r) s = match (E :| Plus r) s
match (Plus r) s = match (r :* Star r) s
match (Opt r) s = match (E :| r) s
match (x :* y) s = concat [match y s' | s' <- match x s]
match (x :| y) s = match x s ++ match y s
match _ _ = []

check :: Regex -> String -> Bool
check r s = "" `elem` match r s

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

regex :: DFA -> Regex
regex d@DFA { alphabet = l, fun = f } = or [α (states d) (start d) x | x <- finals d]
    where α 0 i j = let g | i == j = Opt
                          | otherwise = id
                    in g $ or [ Lit [a] | a <- l, f i a == j ]

          α k i j = let k' = k-1 in α k' i j :| α k' i k :* Star (α k' k k) :* α k' k j

minify :: Regex -> Regex
minify = rewrite $ \x -> case x of
    x :| y | x == y -> Just x
    x :| y | x > y -> Just $ y :| x

    (x :* y) :* z -> Just $ x :* (y :* z)
    (x :| y) :| z -> Just $ x :| (y :| z)

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

    _ -> Nothing

tryReduce :: Eq a => (a -> a) ->  a -> Maybe a
tryReduce f a@(f -> b) | a == b    = Nothing
                       | otherwise = Just b

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

main :: IO ()
main = putStrLn . pretty 0 . minify $ regex goal
