import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (liftIO)

import GHC.IO.Handle
import qualified Data.ByteString as BS

import Network.WebSockets hiding (send, receive)
import Network

client :: Handle -> WebSockets Hybi10 ()
client h = do
  s <- getSink
  liftIO  $ forkIO (send h s)
  forever $ receiveData >>= liftIO . BS.hPut h

send :: Handle -> Sink Hybi10 -> IO ()
send h s = do
  bs <- BS.hGetSome h 1024
  unless (BS.null bs) $ do
    sendSink s (binaryData bs)
    send h s

main :: IO ()
main = do
  (h, _, _) <- listenOn (PortNumber 1194) >>= accept
  hSetBuffering h NoBuffering
  connect "78.46.126.102" 80 "/" (client h)
