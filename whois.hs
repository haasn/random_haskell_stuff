{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)

import Network.Whois
import Web.Scotty

main = scotty 8080 . get "/" $ do
    r <- param "site" >>= liftIO . whois
    text . pack $ unlines (r^..both._Just)
