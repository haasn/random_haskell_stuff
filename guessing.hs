import Control.Monad
import Control.Monad.Maybe
import Control.Monad.IO.Class (liftIO)
import System.Random
import Text.Read

main = do
  n <- randomRIO (0,9 :: Int)
  runMaybeT . forever $ do
    liftIO $ putStrLn "Enter your guess:"
    g <- liftIO readLn
    guard (n /= g)
    liftIO $ putStrLn "Wrong!"
  putStrLn "Right!"
