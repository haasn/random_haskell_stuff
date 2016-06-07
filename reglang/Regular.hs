{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (join)

-- An algebraic data type for regular languages

data Regular a
  = Empty                         -- ∅
  | Singleton a                   -- {A}
  | Union (Regular a) (Regular a) -- A ∪ B
  | Cons (Regular a) (Regular a)  -- A • B
  | Star (Regular a)              -- A*
  deriving Functor

-- Combinators

(<|>) :: Regular a -> Regular a -> Regular a
(<|>) = Union

string :: [a] -> Regular a
string = foldr (Cons . char) Empty

char :: a -> Regular a
char = Singleton

plus :: Regular a -> Regular a
plus = join (Cons . Star)

-- Compilation to Regex

regex :: Regular Char -> String
regex Empty         = ""
regex (Singleton a) = escape a
regex (Union a b)   = wrap (regex a) ++ "|" ++ wrap (regex b)
regex (Cons a b)    = wrap (regex a) ++ wrap (regex b)
regex (Star a)      = wrap (regex a) ++ "*"

wrap :: String -> String
wrap a = "(" ++ a ++ ")"

escape :: Char -> String
escape x
  | x `elem` "[\\^$.|?*+()" = '\\' : [x]
  | otherwise               = [x]

-- An example regex

foo :: Regular Char
foo = plus (string "nand") <|> string "foobar"
