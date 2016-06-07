{-# LANGUAGE RecordWildCards #-}

import Control.Applicative hiding ((<|>))
import Data.Tree
import Data.List (intercalate)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskell)

P.TokenParser{..} = haskell

expr :: Parser (Tree String)
expr = buildExpressionParser table term <?> "expression"
 where
  table = [ [ prefix "-", prefix "+" ]
          , [ postfix "!" ]
          , [ binary "^" AssocRight ]
          , [ binary "*" AssocLeft, binary "/" AssocLeft ]
          , [ binary "+" AssocLeft, binary "-" AssocLeft ]
          ]

  term = (\n -> Node (either show show n) []) <$> naturalOrFloat
         <|> parens expr <?> "term"
  prefix  n  = Prefix  $ Node n . pure <$ reservedOp n
  postfix n  = Postfix $ Node n . pure <$ reservedOp n
  binary n a = Infix ((\a b -> Node n [a,b]) <$ reservedOp n) a

sexpr :: Tree String -> String
sexpr (Node n []) = n
sexpr (Node n xs) = '(' : n ++ ' ' : intercalate " " (map sexpr xs) ++ ")"
