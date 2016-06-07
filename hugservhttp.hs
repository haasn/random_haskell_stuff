{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Applicative
import Data.Monoid ((<>))

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import Prelude hiding (head)
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

main = runSettings s $ \(remoteHost -> h) -> ok . renderHtmlBuilder . html $ do
  head (title "Hug!")
  body $ h1 ("*hugs " <> toHtml (show h) <> "*")
 where
  ok = return . ResponseBuilder ok200 []
  s  = defaultSettings { settingsPort = 2300, settingsHost = HostIPv6 }
