{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad (forever, forM_, void)

import qualified Data.ByteString as BS
import Data.Char (isLetter, chr)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding

import Network.Simple.TCP
import System.IO (hPutStrLn)
import System.IO.Error

main = withSocketsDo $ do
  room <- newBroadcastTChanIO
  let forkRoom = atomically $ dupTChan room
      sendRoom = atomically . writeTChan room

  serve HostAny "6001" $ \(s,_) -> do
    sendTextLn s $ "Hello, what is your name?"
    Just name <- fmap (T.filter isLetter) <$> recvText s 32

    sendRoom $ name <> " connected."
    handling_ _IOException (sendRoom $ name <> " disconnected.") $ do
      -- Listen for messages for as long as we're connected
      forkIO . forever $ do
        Just msg <- recvText s 1024
        sendRoom $ name <> ": " <> msg

      -- Relay messages back to the client
      c <- forkRoom
      forever $ atomically (readTChan c) >>= sendTextLn s

-- network-simple uses ByteString, but Text is a lot more appropriate
recvText s = (fmap.fmap) decodeUtf8 . recv s
sendText s = send s . encodeUtf8
sendTextLn s m = sendText s m >> sendText s "\r\n"
