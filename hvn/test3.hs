{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, GADTs, FlexibleInstances #-}

import Control.Lens
import Data.String

newtype VN a = VN { runVN :: IO a }
  deriving (Functor, Applicative, Monad)

type Style = String -> VN ()

plain :: Style
plain = VN . putStrLn

colored :: String -> Style
colored c = plain . (color ++) where color = "[" ++ c ++ "] "

instance a ~ () => IsString (VN a) where
  fromString = plain

instance (c ~ Style, a ~ ()) => IsString (c -> VN a) where
  fromString = flip id

-- Example character

me  = plain
cat = colored "tabby"

test = do
  "I thought to myself:"
  "this cat seems quite cute"

  cat "nyan"

  me "uguu~"

main = runVN test
