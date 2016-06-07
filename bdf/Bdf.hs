{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.Attoparsec.Text hiding (space)
import Data.Text (pack)

str :: Parser String
str = char '"' *> many (body <|> quoted) <* char '"'
  where quoted = '"' <$ string "\"\""
        body   = satisfy (`notElem` "\r\n\"")

int :: Parser Integer
int = signed decimal

space :: Parser Char
space = char ' '

keyword :: Parser Keyword
keyword =  startFont <|> comment <|> contentVer <|> font <|> size
       <|> fontBoundingBox <|> metricsSet <|> properties <|> chars
 where
  startFont = StartFont <$> kw "STARTFONT" number
  comment = Comment <$> kw "COMMENT" str
  contentVer = ContentVersion <$> kw "CONTENTVERSION" int
  font = Font <$> kw "FONT" str
  size = kw "SIZE" undefined
  fontBoundingBox = kw "FONTBOUNDINGBOX" undefined
  metricsSet = MetricsSet <$> kw "METRICSSET" int

  kw s p = pack s .*> space *> p <?> s

properties :: Parser Keyword
properties = "STARTPROPERTIES" .*> undefined

chars :: Parser Keyword
chars = "CHARS " .*> undefined

-- Internal types

data Keyword
  = StartFont Number
  | Comment String
  | ContentVersion Integer
  | Font String
  | Size () () ()
  | FontBoundingBox () () () ()
  | MetricsSet Integer
  | Properties [(String, Either Integer String)]
  | Chars [Glyph]
  deriving Show

data Glyph = Glyph
  deriving Show
