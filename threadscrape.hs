{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Lens
import System.Environment (getArgs)

import FourChan

main = do
  board : (map read -> threads) <- getArgs
  (concat -> files) <- mapM (getAttachments .: getThread board) threads

(.:) = fmap.fmap
