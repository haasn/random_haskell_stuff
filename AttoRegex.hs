{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Char8

resolves :: Parser Integer
resolves = prefix *> space *> char '#' *> decimal
  where
    prefix = choice $ map string ["fixes", "resolves", "closes"]

findResolves :: Parser Integer
findResolves = resolves <|> anyChar *> findResolves
