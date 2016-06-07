{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens ((??))
import Control.Monad (forever)
import Control.Exception

import GHC.IO.Handle
import Network

main = do
  s <- listenOn (PortNumber 6069)
  let c = try :: IO a -> IO (Either IOException a)
  (finally ?? sClose s) . forever . c $ do
    (h, host, _) <- accept s
    let s = "*hugs " ++ host ++ "*\n"
    hPutStr h s >> putStr s
    hClose h
