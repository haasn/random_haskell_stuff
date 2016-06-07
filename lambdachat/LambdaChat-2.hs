{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)

import Data.Attoparsec hiding (parse)
import Data.ByteString (ByteString, pack)
import Data.Monoid ((<>))

import Pipes
import Pipes.Attoparsec
import Pipes.Network.TCP
import Pipes.Parse
import Pipes.Prelude

main = withSocketsDo $ do
  room <- newBroadcastTChanIO
  let forkRoom = atomically $ dupTChan room
      sendRoom = atomically . writeTChan room

  serve HostAny "6001" $ \(s,_) -> do
    let lines = void . parseMany line $ fromSocket s 4096

    sendLn s $ "Hello, what is your name?"
    (Right (_,name), rest) <- runStateT draw lines -- parse a single line

    sendRoom $ name <> " connected."
    c <- forkRoom

    -- Relay messages back to the client
    forkIO . forever $ atomically (readTChan c) >>= sendLn s
    -- Listen for messages for as long as we're connected
    runEffect . for lines $ \(_, m) -> lift . sendRoom $ name <> ": " <> m
    -- Clean up.
    sendRoom $ name <> " disconnected."

  where sendLn s m = send s m >> send s "\n"

-- FIXME: parse a Text instead of handling UTF-8 values directly
line :: Parser ByteString
line = pack <$> many1 (notWord8 10) <* word8 10 -- \n
