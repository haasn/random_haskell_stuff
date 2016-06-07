import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (liftIO)

import GHC.IO.Handle
import qualified Data.ByteString as BS

import Network.WebSockets hiding (send, receive)
import Network

server :: Request -> WebSockets Hybi10 ()
server r = do
  acceptRequest r
  s <- getSink

  h <- liftIO (connectTo "127.0.0.1" (PortNumber 1194))
  liftIO (hSetBuffering h NoBuffering)

  liftIO (forkIO (send h s))
  forever $ receiveData >>= liftIO . BS.hPut h

send :: Handle -> Sink Hybi10 -> IO ()
send h s = do
  bs <- BS.hGetSome h 1024
  unless (BS.null bs) $ do
    sendSink s (binaryData bs)
    send h s

main :: IO ()
main = runServer "0.0.0.0" 80 server
