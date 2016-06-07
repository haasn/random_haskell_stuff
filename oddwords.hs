{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.List.Split
import Control.Lens

oddWords :: String -> String
oddWords = g . (each.elements odd %~ reverse) . f
  where f = map words . splitOn "."
        g = intercalate "." . map unwords
